{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( wallet
    , owner2pkh
    , owner1pkh
    , polT
    --testnet
    , testnetOwner1Pkh
    , testnetOwner2Pkh
    ) where


import           Plutus.V1.Ledger.Crypto          (PubKeyHash)
import           Wallet.Emulator.Wallet           (Wallet, fromWalletNumber, walletPubKeyHash)
import           Wallet.Emulator.Types            (WalletNumber (..))
import           Plutus.V1.Ledger.Value           (CurrencySymbol (..))
import           PlutusTx.Prelude                 ((.))
import           Prelude hiding                   ((.))


wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

owner1pkh :: PubKeyHash
owner1pkh = walletPubKeyHash $ wallet 1

owner2pkh :: PubKeyHash
owner2pkh = walletPubKeyHash $ wallet 2

polT :: CurrencySymbol
polT = CurrencySymbol "test"

--testnet
testnetOwner1Pkh :: PubKeyHash
testnetOwner1Pkh = "3795730786fe4960e5d6750ac3e5e91350181ab874828d79ecaefc91"

testnetOwner2Pkh :: PubKeyHash
testnetOwner2Pkh = "0b83b3a70145fcb7936491e8ee22bcdb8cf235be855c2f64eb016665"



