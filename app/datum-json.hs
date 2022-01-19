{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson             as Json (encode)
import           Data.ByteString.Lazy   qualified as LB
import           System.Environment     (getArgs)
import           Prelude
import           Data.String            (fromString)
import           Plutus.V1.Ledger.Time  (DiffMilliSeconds (..), fromMilliSeconds)
import           Cardano.Api            (scriptDataToJson, ScriptDataJsonSchema(ScriptDataJsonDetailedSchema))
import           Cardano.Api.Shelley    (fromPlutusData)
import qualified PlutusTx

import           Auction.Types          (Auction(..))

-- This module is here to convert Haskell Data Types to JSON files, particularly used for Auction custom Datum Type.
-- To use this enter `cabal run datum-json <seller> <owner1> <owner2> <deadline> <minBid> <policy> <tn>`. (price in lovelace, seller/owner1/owner2 as pubKeyHash, deadline in MilliSeconds)
-- You can use ./getPubKeyHash <wallet name> to get your wallet PubKeyHash 

-- Constructs the JSON file for the datum, used as input to --tx-in-datum-file in cardano-cli

main :: IO ()
main = do
  [seller', owner1', owner2', deadline', minBid', policy', token'] <- getArgs
  let seller     = fromString seller'
      owner1     = fromString owner1'
      owner2     = fromString owner2'
      deadline   = fromMilliSeconds $ DiffMilliSeconds (read deadline')
      minBid     = read minBid'
      policy     = fromString policy'
      token      = fromString token'
      auctionEx  = Auction seller owner1 owner2 deadline minBid policy token
  writeData ("testnet/datum-"  ++ token' ++ ".json") auctionEx
  putStrLn "Done"


-- Datum also needs to be passed when sending the token to the script (aka putting for start)
-- When doing this, the datum needs to be hashed, see Alonzo-purple exercise-solutions on how to hash a datum


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
