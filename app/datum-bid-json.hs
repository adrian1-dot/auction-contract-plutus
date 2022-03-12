{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson             as Json (encode)
import           Data.ByteString.Lazy   qualified as LB
import           System.Environment     (getArgs)
import           Prelude
import           Data.String            (fromString)
import           Plutus.V1.Ledger.Time  (DiffMilliSeconds (..), fromMilliSeconds, POSIXTime (..))
import           Ledger.TimeSlot        (SlotConfig (..), slotToEndPOSIXTime)
import           Cardano.Api            (scriptDataToJson, ScriptDataJsonSchema(ScriptDataJsonDetailedSchema))
import           Cardano.Api.Shelley    (fromPlutusData)
import           Ledger                 (PaymentPubKeyHash (..))
import qualified PlutusTx

import           Auction.Types          (Auction(..), AuctionDatum(..), Bid (..), AuctionAction (..))

-- This module is here to convert Haskell Data Types to JSON files, particularly used for Auction custom Datum Type.
-- To use this enter `cabal run datum-json <seller> <deadline> <minBid> <policy> <tn>`. (price in lovelace, seller as pubKeyHash, deadline in MilliSeconds)
-- You can use `cardano-cli address key-hash --payment-verification-key-file /path-to-your-vkey-file` to get your wallet PubKeyHash 

-- Constructs the JSON file for the datum, used as input to --tx-in-datum-file

main :: IO ()
main = do
  [seller', deadline', minBid', policy', token', bidder', bidAmount'] <- getArgs
  let seller     = PaymentPubKeyHash {unPaymentPubKeyHash = (fromString seller')}
      deadline   = fromMilliSeconds $ DiffMilliSeconds (read deadline')
      minBid     = read minBid'
      policy     = fromString policy'
      token      = fromString token'
      bidder     = PaymentPubKeyHash {unPaymentPubKeyHash = (fromString bidder')}
      bidAmount  = read bidAmount'
      bidEx      = Bid bidder bidAmount
      auctionEx  = Auction seller deadline minBid policy token
      auctionDatum = AuctionDatum auctionEx $ Just bidEx
  writeData ("datum-"  ++ token' ++ "-bid.json") auctionDatum
  writeData ("bid-" ++ token' ++ ".json") (MkBid bidEx)
  putStrLn "Done"


writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
