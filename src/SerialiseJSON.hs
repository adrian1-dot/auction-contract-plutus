{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SerialiseJSON (testR, testD) where

import           Data.Aeson                 as Json ( encode )
import           Data.ByteString.Lazy       qualified as LB


import           Cardano.Api                (scriptDataToJson, ScriptDataJsonSchema(ScriptDataJsonDetailedSchema))
import           Cardano.Api.Shelley        (fromPlutusData)
import qualified PlutusTx
import           Plutus.V1.Ledger.Time      (POSIXTime (..))
import           Ledger.TimeSlot            (slotToEndPOSIXTime)
import           Data.Default               (def, Default (..))
import           Ledger.Address             (PaymentPubKeyHash (..))

import           Auction.Types              (Auction (..), AuctionAction (..), Bid (..), AuctionDatum (..))

-- This module is here to convert Haskell Data Types to JSON files, particularly used for Redeemer and Datum
-- To use this enter `cabal repl` ; `:l src/SerialiseJSON.hs` ; `testR` or `testD`


testR :: IO ()
testR = do 
  writeData "bidCorrect.json" (MkBid bidEx)
  putStrLn "Done"


bidEx :: Bid 
bidEx = Bid 
     { bBidder = (PaymentPubKeyHash "3795730786fe4960e5d6750ac3e5e91350181ab874828d79ecaefc91")
     , bBid    = 1
     }

testTime :: POSIXTime
testTime = slotToEndPOSIXTime def 10

auctionEx :: Auction
auctionEx = Auction
     { aSeller       = (PaymentPubKeyHash "3795730786fe4960e5d6750ac3e5e91350181ab874828d79ecaefc91")
     , aDeadline     = testTime
     , aMinBid       = 1
     , aPolicy       = "2b23e423a5ab5de9bed02187ee2e240e8ea0a21bd3dbe2c498ec75d0"
     , aTokenName    = "test"
     }

      -- This is an example to fill with real data
 
testD :: IO ()
testD = do
  writeData "testnet/datum.json" auctionEx
  putStrLn "Done"
-- Datum also needs to be passed when sending the token to the script (aka putting for sale)
-- When doing this, the datum needs to be hashed, see Alonzo-purple exercise-solutions on how to hash a datum
  
writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
