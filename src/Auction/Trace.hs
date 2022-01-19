{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Auction.Trace
    (startTest, bidTest, closeTest, test) where


import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           Control.Monad    (void)
import           PlutusTx.Prelude as Plutus ( ($), (<>), Either(..))
import           Ledger.Value     as Value (singleton, Value (..))
import qualified Data.Map         as Map
import qualified Ledger.Ada       as Ada
import           Ledger.TimeSlot 
import           Prelude      (IO)
import           Data.Default (def, Default (..))
import           Plutus.V1.Ledger.Time (POSIXTime (..))
import           Wallet.Emulator.Wallet (Wallet (..))

import           Auction.Types 
import           Auction.Offchain
import           Utility (wallet, polT)

nftEx1 :: StartParams
nftEx1 = StartParams { spDeadline = testTime, spMinBid = 10_000_000, spCurrency = polT, spToken = "Test1"}

nftExBid1 :: BidParams
nftExBid1 = BidParams { bpCurrency = polT, bpToken = "Test1", bpBid = 11_000_000}

nftExClose1 :: CloseParams
nftExClose1 = CloseParams { cpCurrency = polT, cpToken = "Test1"}

testTime :: POSIXTime
testTime = slotToEndPOSIXTime def 10 

dist :: Map.Map Wallet Value
dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 2, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 3, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 4, Ada.lovelaceValueOf 100_000_000
                                      <> Value.singleton polT "Test1" 1
                                      <> Value.singleton polT "Test2" 1)
                            , (wallet 5, Ada.lovelaceValueOf 100_000_000)
                            ]
emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left dist) def def 

startTest :: IO ()
startTest = 
    runEmulatorTraceIO' def emCfg $ do 
        h4 <- activateContractWallet (wallet 4) endpoints
        void $ Emulator.waitNSlots 1 
        callEndpoint @"start" h4 nftEx1
        void $ Emulator.waitNSlots 1

bidTest :: IO ()
bidTest =  
    runEmulatorTraceIO' def emCfg $ do 
        h4 <- activateContractWallet (wallet 4) endpoints
        h5 <- activateContractWallet (wallet 5) endpoints
        void $ Emulator.waitNSlots 1 
        callEndpoint @"start" h4 nftEx1 
        void $ Emulator.waitNSlots 1 
        callEndpoint @"bid" h5 nftExBid1 
        void $ Emulator.waitNSlots 1 

closeTest :: IO ()
closeTest =  
    runEmulatorTraceIO' def emCfg $ do 
        h4 <- activateContractWallet (wallet 4) endpoints
        h5 <- activateContractWallet (wallet 5) endpoints
        void $ Emulator.waitNSlots 1 
        callEndpoint @"start" h4 nftEx1 
        void $ Emulator.waitNSlots 1
        callEndpoint @"bid" h5 nftExBid1
        void $ Emulator.waitNSlots 10
        callEndpoint @"close" h4 nftExClose1
        void $ Emulator.waitNSlots 1

test :: IO () 
test = 
    runEmulatorTraceIO' def emCfg $ do 
        h4 <- activateContractWallet (wallet 4) endpoints
        h5 <- activateContractWallet (wallet 5) endpoints 
        void $ Emulator.waitNSlots 1 
        callEndpoint @"start" h4 nftEx1
        void $ Emulator.waitNSlots 1 
        callEndpoint @"bid" h5 nftExBid1
        void $ Emulator.waitNSlots 10 
        callEndpoint @"close" h4 nftExClose1
        void $ Emulator.waitNSlots 1







