#example input: ./mkBid.sh TestToken wallet1 100000000 refundWallet

tn=$1
bidAmount=$3
seller=`cat datum-$tn.json | jq -r '.fields[0].fields[0].bytes'`
deadline=`cat datum-$tn.json | jq '.fields[0].fields[1].int'`
minBid=`cat datum-$tn.json | jq '.fields[0].fields[2].int'`
policy=`cat datum-$tn.json | jq -r '.fields[0].fields[3].bytes'`
ownPkh=$(cat wallets/$2.pkh)


cabal run datum-bid-json $seller $deadline $minBid $policy $tn $ownPkh $bidAmount

source getTxFunc.sh

getInputTx $2
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}

getInputTx ../auction
CON_FROM_UTXO=${SELECTED_UTXO}
FROM_CON_NAME=${SELECTED_WALLET_NAME}
FROM_CON_ADDRESS=${SELECTED_WALLET_ADDR}

slot=`cardano-cli query tip $TESTNET | jq '.slot'`
slot=`expr $slot + 1000`
echo $slot > slotnumber
hexTn=$(cat datum-$1.json | jq -r '.fields[0].fields[4].bytes')

outVal=`expr $bidAmount + 2000000`
maybeBid=$(cat datum-$1.json | jq '.fields[1].fields')

if [[ $maybeBid = [] ]]
then 
    tx=`cardano-cli transaction build --alonzo-era $TESTNET --change-address ${FROM_WALLET_ADDRESS} --tx-in ${FROM_UTXO} --tx-in ${CON_FROM_UTXO} --tx-in-script-file auction.plutus --tx-in-datum-file datum-$1.json --tx-in-redeemer-file bid-$tn.json --invalid-hereafter $(cat slotnumber) --tx-in-collateral ${FROM_UTXO} --tx-out "${FROM_CON_ADDRESS}+ $outVal +1 $policy.$hexTn" --tx-out-datum-embed-file datum-$1-bid.json --protocol-params-file protocol.json --out-file tx/bid.tx`
    rm datum-$tn.json 
    mv datum-$tn-bid.json datum-$tn.json
else
    price=$(cat datum-$1.json | jq '.fields[1].fields[0].fields[1].int')
    tx=`cardano-cli transaction build --alonzo-era $TESTNET --tx-in ${FROM_UTXO} --tx-in ${CON_FROM_UTXO} --tx-in-script-file auction.plutus --tx-in-datum-file datum-$1.json --tx-in-redeemer-file bid-$tn.json  --tx-in-collateral ${FROM_UTXO} --invalid-hereafter $(cat slotnumber) --tx-out "${FROM_CON_ADDRESS} + $outVal + 1 $policy.$hexTn"   --tx-out-datum-embed-file datum-$1-bid.json --tx-out "$(cat wallets/$4.addr) + $price" --change-address ${FROM_WALLET_ADDRESS} --protocol-params-file protocol.json --out-file tx/bid.tx`
    rm datum-$tn.json 
    mv datum-$tn-bid.json datum-$tn.json
fi


cardano-cli transaction sign \
   --tx-body-file tx/bid.tx \
   --signing-key-file wallets/${FROM_WALLET_NAME}.skey \
   $TESTNET \
   --out-file tx/bid.signed

cardano-cli transaction submit --tx-file tx/bid.signed $TESTNET
