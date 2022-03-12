# Auction Contract 

This repository is built upon the [EnglishAuction](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week01/src/Week01/EnglishAuction.hs) example from the plutus-pioneer-program. 

## Setting up

Set up the [plutus platform](https://github.com/input-output-hk/plutus-apps).

## Test locally 

* run `cabal build`
* run `cabal repl`
* run `import Auction.Trace`
* now you can run a trace for every endpoint (startTest, bidTest, closeTest) and test to show a full example. 


## Testnet (cardano-cli)

#### offer NFT

* Install and run a [`cardano-node`](https://github.com/input-output-hk/cardano-node) on the testnet, you'll now have a node.socket file. Put the path to this file in the env variable `CARDANO_NODE_SOCKET_PATH`
* run `TESTNET=--testnet testnet-magic 1097911063`
* save your vkey, skey, addr files in testnet/wallets/ 
* run `cardano-cli address key-hash --payment-verification-key-file /pathToYour/vkey/file` to get your PubKeyHash
* build the project with `cabal build` (/auction)
* run `cabal run auction-plutus`
* copy the resulting `auction.plutus` file in the testnet folder 
* run `cardano-cli address build --payment-script-file auction-testnet.plutus $TESTNET > auction-testnet.addr`
* run `./offerToken.sh tokenName your_wallet_name CurrencySymbol deadline(in minutes) minBid(in Lovelace)`

#### make a bid 

* run `./mkBid.sh tokenName your_wallet_name price bidAmount wallet_to_refund` 

#### close the Sale 

* run `./closeSale.sh tokenName your_wallet_name`


