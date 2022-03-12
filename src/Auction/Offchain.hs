{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Auction.Offchain (endpoints) where 

import qualified Data.Map                  as Map
import           Data.Monoid               as Mnd ( (<>) )
import           Control.Monad             hiding (fmap)
import           Data.Aeson                ()
import           Data.Text                 (Text, pack)
import           Prelude                   (String, fromIntegral, ceiling, Float, (*), show)


import           Plutus.Contract          as Contract (AsContractError, logInfo, throwError, awaitTxConfirmed, endpoint, submitTxConstraintsWith,
                                          utxosTxOutTxAt, select, Contract, Promise(awaitPromise), ownPaymentPubKeyHash, submitTxConstraints)
import qualified PlutusTx
import           PlutusTx.Prelude         as Plutus  hiding (Semigroup (..), unless)
import           Ledger                   (scriptAddress, to, from, txOutDatumHash, CurrencySymbol, TokenName, Redeemer(Redeemer),
                                          TxOutRef, ChainIndexTxOut (..), toTxOut, getCardanoTxId)
import           Ledger.Constraints       as Constraints (otherScript, typedValidatorLookups, unspentOutputs, mustPayToPubKey, mustPayToTheScript,
                                          mustSpendScriptOutput, mustValidateIn)
import           Ledger.Value             as Value (singleton, valueOf)
import qualified Plutus.V1.Ledger.Ada     as Ada (lovelaceValueOf)
import           Plutus.ChainIndex.Tx     (ChainIndexTx(_citxData))

import           Auction.Types            (StartParams (..), BidParams (..), CloseParams (..), Datum (..), AuctionSchema, Auction (..), AuctionDatum (..), Bid (..), AuctionAction (..))
import           Text.Printf              (printf)

import           Auction.Onchain 
import           Utility                  (feePayment)
import           Auction.Types            (FeePayment (..))

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
    pkh <- Contract.ownPaymentPubKeyHash
    let a = Auction
                { aSeller       = pkh
                , aDeadline     = spDeadline
                , aMinBid       = spMinBid
                , aPolicy       = spCurrency
                , aTokenName = spToken
                }
        d = AuctionDatum
                { adAuction    = a
                , adHighestBid = Nothing
                }
        v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf 2000000
        tx = mustPayToTheScript d v
    ledgerTx <- submitTxConstraints (auctionTypedValidator feePayment) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "started auction %s for token %s" (show a) (show v)

bid :: forall w s. BidParams -> Contract w s Text ()
bid BidParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction bpCurrency bpToken
    logInfo @String $ printf "found auction utxo with datum %s" (show d)

    when (bpBid < minBid d) $
        throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
    pkh <- Contract.ownPaymentPubKeyHash
    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf (2000000 + bpBid)
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

        lookups = Constraints.typedValidatorLookups (auctionTypedValidator feePayment) <>
                  Constraints.otherScript (auctionValidator feePayment)                <>
                  Constraints.unspentOutputs (Map.singleton oref o)
    let tx = case adHighestBid of
                    Nothing      -> mustPayToTheScript d' v    <>
                                    mustValidateIn (to $ aDeadline adAuction)          <>
                                    mustSpendScriptOutput oref r
                    Just Bid{..} -> mustPayToTheScript d' v                            <>
                                    mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
                                    mustValidateIn (to $ aDeadline adAuction)          <>
                                    mustSpendScriptOutput oref r
    logInfo @String $ "datum " <> show d'
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made bid of %d lovelace in auction %s for token (%s, %s)"
        bpBid
        (show adAuction)
        (show bpCurrency)
        (show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction cpCurrency cpToken
    logInfo @String $ printf "found auction utxo with datum %s" (show d)
    let t      = Value.singleton cpCurrency cpToken 1 
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction
        lookups = Constraints.typedValidatorLookups (auctionTypedValidator feePayment) <>
                  Constraints.otherScript (auctionValidator feePayment)                <>
                  Constraints.unspentOutputs (Map.singleton oref o)
    case adHighestBid of
        Nothing      -> do 
                let tx = mustPayToPubKey seller (t <> Ada.lovelaceValueOf 2000000)           <>
                         mustValidateIn (from $ aDeadline adAuction)       <>
                         mustSpendScriptOutput oref r
                ledgerTx <- submitTxConstraintsWith lookups tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "dead: " <> show (from $ aDeadline adAuction)
        Just Bid{..} -> do
                let valAdaF = Ada.lovelaceValueOf $ minV (ceiling (0.02 Prelude.* fromIntegral bBid :: Float))    
                    tx      = mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf 2000000)  <>
                              mustPayToPubKey seller (Ada.lovelaceValueOf bBid) <>
                              mustPayToPubKey (aOwner1Pkh feePayment) valAdaF               <>
                              mustPayToPubKey (aOwner2Pkh feePayment) valAdaF               <>
                              mustValidateIn (from $ aDeadline adAuction)       <>
                              mustSpendScriptOutput oref r
                ledgerTx <- submitTxConstraintsWith lookups tx
                logInfo @String $ "valAdaF: " <> show valAdaF
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "closed auction %s for token (%s, %s)"
        (show adAuction)
        (show cpCurrency)
        (show cpToken)


minV :: Integer -> Integer 
minV i = if ( i > 2000000 ) then i else 2000000

findAuction :: CurrencySymbol -> TokenName -> Contract w s Text (TxOutRef, ChainIndexTxOut, AuctionDatum)
findAuction cs tn = do
    utxos <- utxosTxOutTxAt $ scriptAddress (auctionValidator feePayment)
    let xs = [ (oref, (utxo, tx))
             | (oref, (utxo, tx)) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue utxo) cs tn == 1
             ]
    case xs of
        [(oref, (utxo, tx))] -> case txOutDatumHash $ toTxOut utxo of
            Nothing   -> throwError "unexpected out type"
            Just h -> case Map.lookup h $ _citxData tx of
                Nothing        -> throwError "datum not found"
                Just (Datum e) -> case PlutusTx.fromBuiltinData e of
                    Nothing -> throwError "datum has wrong type"
                    Just d@AuctionDatum{..}
                        | aPolicy adAuction == cs && aTokenName adAuction == tn -> return (oref, utxo, d)
                        | otherwise                                           -> throwError "auction token missmatch"
        _           -> throwError "auction utxo not found"



endpoints :: Contract () AuctionSchema Text ()
endpoints = forever 
           $ awaitPromise
           $ start' `select` bid' `select` close'
 where
   start' = endpoint @"start"  start
   bid'   = endpoint @"bid"    bid
   close' = endpoint @"close"  close


