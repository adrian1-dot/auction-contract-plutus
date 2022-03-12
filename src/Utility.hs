{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( wallet
    , feePayment
    , polT
    --testnet
    , testnetFeePayment
    ) where


import           Ledger                           (PaymentPubKeyHash (..))
import           Wallet.Emulator.Wallet           (Wallet, fromWalletNumber, mockWalletPaymentPubKeyHash)
import           Wallet.Emulator.Types            (WalletNumber (..), knownWallet)
import           Plutus.V1.Ledger.Value           (CurrencySymbol (..))
import           PlutusTx.Prelude                 ((.))
import           Prelude hiding                   ((.))

import           Auction.Types                    (FeePayment (..))

feePayment :: FeePayment 
feePayment = FeePayment
    { aOwner1Pkh = owner1pkh
    , aOwner2Pkh = owner2pkh
    }

wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

owner1pkh :: PaymentPubKeyHash
owner1pkh = mockWalletPaymentPubKeyHash $ knownWallet 1

owner2pkh :: PaymentPubKeyHash
owner2pkh = mockWalletPaymentPubKeyHash $ knownWallet 2

polT :: CurrencySymbol
polT = CurrencySymbol "test"

--testnet
testnetFeePayment :: FeePayment
testnetFeePayment = FeePayment
    { aOwner1Pkh = testnetOwner1Pkh
    , aOwner2Pkh = testnetOwner2Pkh
    }

testnetOwner1Pkh :: PaymentPubKeyHash
testnetOwner1Pkh = PaymentPubKeyHash "c994a4a7e265253f4c2383882cb1eac08fbf0c1a1bbff7f5a00996b5"

testnetOwner2Pkh :: PaymentPubKeyHash
testnetOwner2Pkh = PaymentPubKeyHash "f156560859c7cc4c5c68de5003c7bb36276007c75155c924d274b611"



