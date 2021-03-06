{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Auction.Onchain where

import           PlutusTx.Ratio           ((%))
import           Codec.Serialise          (serialise)
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Control.Monad            hiding (fmap)
import           Data.Aeson               (ToJSON, FromJSON)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Map                 as Map
import           Data.Text                (pack, Text)
import           GHC.Generics             (Generic)
import           Plutus.Contract          as Contract
import qualified PlutusTx                 as PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude         as Plutus
import           Plutus.ChainIndex.Tx     (ChainIndexTx (..))
import           Ledger                   hiding (singleton, fee)
import           Ledger.Constraints       as Constraints
import qualified Ledger.Scripts           as Scripts
import qualified Ledger.Typed.Scripts     as Scripts hiding (validatorHash)
import           Ledger.Value             as Value
import           Ledger.Ada               as Ada
import           Playground.Contract      (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH            (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types         (KnownCurrency (..))
import           Prelude                  (IO, Semigroup (..), Show (..), String)
import           Schema                   (ToSchema)
import           Text.Printf              (printf)
import qualified Plutus.V1.Ledger.Scripts as Plutus

import           Auction.Types            (Auction (..), Bid (..), AuctionAction (..), Auctioning, FeePayment (..), AuctionDatum (..))

minLovelace :: Integer
minLovelace = 2000000

{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: FeePayment -> AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator fee ad redeemer ctx =
    traceIfFalse "wrong input value" correctInputValue &&
    case redeemer of
        MkBid b@Bid{..} ->
            traceIfFalse "bid too low" (sufficientBid bBid)                &&
            traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund"       correctBidRefund             &&
            traceIfFalse "too late"           correctBidSlotRange
        Close           ->
            traceIfFalse "too early" correctCloseSlotRange &&
            case adHighestBid ad of
                Nothing      -> 
                    traceIfFalse "expected seller to get token" (getsValue seller $ tokenValue <> Ada.lovelaceValueOf minLovelace)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid) &&
                    traceIfFalse "Fee not paid" (checkFee bBid)

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    auction :: Auction
    auction = adAuction ad
    
    seller :: PaymentPubKeyHash
    seller = aSeller auction 

    tokenValue :: Value
    tokenValue = Value.singleton (aPolicy auction) (aTokenName auction) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case adHighestBid ad of
        Nothing      -> tokenValue Plutus.<> Ada.lovelaceValueOf minLovelace
        Just Bid{..} -> tokenValue Plutus.<> Ada.lovelaceValueOf (minLovelace + bBid)

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput   :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = (adAuction outputDatum == auction)   &&
                              (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
        txOutValue ownOutput == tokenValue Plutus.<> Ada.lovelaceValueOf (amount + minLovelace)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
        Nothing      -> True
        Just Bid{..} ->
          let
            os = [ o
                 | o <- txInfoOutputs info
                 , txOutAddress o == pubKeyHashAddress bBidder Nothing
                 ]
          in
            case os of
                [o] -> txOutValue o == Ada.lovelaceValueOf bBid
                _   -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = contains (to $ aDeadline auction) $ txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = contains (from $ aDeadline auction) $ txInfoValidRange info

    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

    checkFee :: Integer -> Bool
    checkFee price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info (unPaymentPubKeyHash $ aOwner1Pkh fee)))) >= 2 % 100 * fromInteger price &&
               fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info (unPaymentPubKeyHash $ aOwner2Pkh fee)))) >= 2 % 100 * fromInteger price 



auctionTypedValidator :: FeePayment -> Scripts.TypedValidator Auctioning
auctionTypedValidator fee = Scripts.mkTypedValidator @Auctioning
    ($$(PlutusTx.compile [|| mkAuctionValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode fee)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction

auctionValidator :: FeePayment -> Validator
auctionValidator = Scripts.validatorScript . auctionTypedValidator

auctionAddress :: FeePayment -> Ledger.ValidatorHash
auctionAddress = Scripts.validatorHash . auctionValidator

auctionScript :: FeePayment -> Plutus.Script
auctionScript = Ledger.unValidatorScript . auctionValidator

auctionScriptAsShortBs :: FeePayment -> SBS.ShortByteString
auctionScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . auctionScript

apiAuctionScript :: FeePayment -> PlutusScript PlutusScriptV1
apiAuctionScript = PlutusScriptSerialised . auctionScriptAsShortBs
