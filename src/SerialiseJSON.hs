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

import           Auction.Types              (Auction (..), AuctionAction (..), Bid (..))

-- This module is here to convert Haskell Data Types to JSON files, particularly used for Redeemer and Datum
-- To use this enter `cabal repl` ; `:l src/SerialiseJSON.hs` ; `testR` or `testD`


-- Constructs the JSON file for the Close Redeemer constructor, used as input to --tx-in-redeemer-file
testR :: IO ()
testR = do 
  writeData "close.json" Close
  putStrLn "Done"


bidEx :: Bid 
bidEx = Bid 
     { bBidder = "pass"
     , bBid    = 1
     }

testTime :: POSIXTime
testTime = slotToEndPOSIXTime def 10

auctionEx :: Auction
auctionEx = Auction
     { aSeller       = "pass"
     , aOwner1Pkh    = "pass"
     , aOwner2Pkh    = "pass"
     , aDeadline     = testTime
     , aMinBid       = 1
     , aPolicy       = "pass"
     , aTokenName    = "pass"
     }

      -- This is an example to fill with real data
      -- The `aSeller` needs to be in Base16 format, not Bech32 (addr1...).
      -- To easily get the Base16 version, go to Cardanoscan.io, search the address in format addr1...
       -- The address is written in two formats, the first being Bech32 aka addr1... and the other (in light gray) being in Base16
 
-- Constructs the JSON file for the nftEx datum, used as input to --tx-in-datum-file
testD :: IO ()
testD = do
  writeData "datum.json" auctionEx
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
