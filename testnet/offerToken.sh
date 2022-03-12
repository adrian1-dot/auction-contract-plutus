#example input: ./offerToken.sh TestToken wallet1 policy deadline(in minutes) minBid(in Lovelace)  

./getDeadline.sh $4

tn=$1
wallet=$2 
policy=$3
deadline=$(cat $4-min.time)
minBid=$5


cabal run datum-json $(cat wallets/$wallet.pkh) $deadline $minBid $policy $tn

source getTxFunc.sh

getInputTx $wallet
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
 
hexTn=$(cat datum-$tn.json | jq -r '.fields[0].fields[4].bytes') 
 
cardano-cli transaction hash-script-data --script-data-file datum-$tn.json > dhash$tn

cardano-cli transaction build \
     --alonzo-era \
     $TESTNET \
     --tx-in ${FROM_UTXO} \
     --tx-out "$(cat auction.addr) + 2000000 + 1 $policy.$hexTn" \
     --tx-out-datum-hash $(cat dhash$tn) \
     --change-address ${FROM_WALLET_ADDRESS} \
     --protocol-params-file protocol.json \
     --out-file tx/tx$tn.draft 
 
 
cardano-cli transaction sign \
     --tx-body-file tx/tx$tn.draft \
     --signing-key-file wallets/${FROM_WALLET_NAME}.skey \
     $TESTNET \
     --out-file tx/tx$tn.signed 
 
cardano-cli transaction submit --tx-file tx/tx$tn.signed $TESTNET
