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

import           Auction.Types          (Auction(..), AuctionDatum(..), Bid (..))

-- This module is here to convert Haskell Data Types to JSON files, particularly used for Auction custom Datum Type.
-- To use this enter `cabal run datum-json <seller> <owner1> <owner2> <deadline> <minBid> <policy> <tn>`. (price in lovelace, seller/owner1/owner2 as pubKeyHash, deadline in MilliSeconds)
-- You can use --payment-verification-key-file wallets/wallet1.vkey to get your wallet PubKeyHash

-- Constructs the JSON file for the datum, used as input to --tx-in-datum-file

main :: IO ()
main = do
  [seller', deadline', minBid', policy', token'] <- getArgs
  let seller     = PaymentPubKeyHash {unPaymentPubKeyHash = (fromString seller')}
      deadline   = fromMilliSeconds $ DiffMilliSeconds (read deadline')
      minBid     = read minBid'
      policy     = fromString policy'
      token      = fromString token'
      auctionEx  = Auction seller deadline minBid policy token
      auctionDatum = AuctionDatum auctionEx Nothing
  writeData ("datum-"  ++ token' ++ ".json") auctionDatum
  putStrLn "Done"



testnetConfig :: SlotConfig
testnetConfig = SlotConfig 1000 timeWhenSlotChangedTo1sec

timeWhenSlotChangedTo1sec :: POSIXTime
timeWhenSlotChangedTo1sec = POSIXTime 1595967616000 

testTime :: POSIXTime                                                           
testTime = slotToEndPOSIXTime testnetConfig 48953889

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
