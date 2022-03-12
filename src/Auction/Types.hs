{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}

module Auction.Types 
    (Auction (..)
    , Bid (..)
    , AuctionDatum (..)
    , AuctionAction (..)
    , Auctioning
    , StartParams (..)
    , BidParams (..)
    , CloseParams (..)
    , Datum (..)
    , AuctionSchema
    , FeePayment (..)
    ) where 

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Prelude                   (Show (..))
import qualified Prelude                   as Pr

import           Schema                    (ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Eq(..), (&&), Integer)
import           Ledger                    ( TokenName, CurrencySymbol, PaymentPubKeyHash, POSIXTime)
import           Ledger.Scripts            as Scripts
import           Ledger.Typed.Scripts      as Scripts hiding (validatorHash)
import           Plutus.Contract           ( Endpoint, type (.\/) )

data FeePayment = FeePayment 
    { aOwner1Pkh :: !PaymentPubKeyHash
    , aOwner2Pkh :: !PaymentPubKeyHash
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''FeePayment

data Auction = Auction
    { aSeller          :: !PaymentPubKeyHash
    , aDeadline        :: !POSIXTime
    , aMinBid          :: !Integer
    , aPolicy          :: !CurrencySymbol
    , aTokenName       :: !TokenName
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Auction where
    {-# INLINABLE (==) #-}
    a == b = (aSeller       a == aSeller       b) &&
             (aDeadline     a == aDeadline     b) &&
             (aMinBid       a == aMinBid       b) &&
             (aPolicy       a == aPolicy       b) &&
             (aTokenName    a == aTokenName    b)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
    { bBidder :: !PaymentPubKeyHash
    , bBid    :: !Integer
    } deriving Show

instance Eq Bid where
    {-# INLINABLE (==) #-}
    b == c = (bBidder b == bBidder c) &&
             (bBid    b == bBid    c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = MkBid Bid | Close
    deriving Show

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum
    { adAuction    :: !Auction
    , adHighestBid :: !(Pr.Maybe Bid)
    } deriving Show

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

data Auctioning
instance Scripts.ValidatorTypes Auctioning where
    type instance RedeemerType Auctioning = AuctionAction
    type instance DatumType Auctioning = AuctionDatum

--Offchain 
data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams
    { bpCurrency :: !CurrencySymbol
    , bpToken    :: !TokenName
    , bpBid      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams
    { cpCurrency :: !CurrencySymbol
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =
            Endpoint "start" StartParams
        .\/ Endpoint "bid"   BidParams
        .\/ Endpoint "close" CloseParams



