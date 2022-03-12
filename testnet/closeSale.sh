#example input: ./closeSale.sh token1 wallet1 
   
source getTxFunc.sh
   
getInputTx $2
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
   
    
getInputTx ../auction
CON_FROM_UTXO=${SELECTED_UTXO}
CON_FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
CON_FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
 

hexTn=$(cat datum-$1.json | jq -r '.fields[0].fields[4].bytes') 
hexCs=$(cat datum-$1.json | jq -r '.fields[0].fields[3].bytes')
isBid=$(cat datum-$1.json | jq '.fields[1].fields')

slot=`cardano-cli query tip $TESTNET | jq '.slot'`


if [[ $isBid = [] ]]
then
    tx=`cardano-cli transaction build --alonzo-era $TESTNET --change-address ${FROM_WALLET_ADDRESS} --tx-in ${FROM_UTXO} --tx-in ${CON_FROM_UTXO} --tx-in-script-file auction.plutus --tx-in-datum-file datum-$1.json --tx-in-redeemer-file close.json --tx-in-collateral ${FROM_UTXO} --invalid-before $slot --tx-out "${FROM_WALLET_ADDRESS} + 2000000 + 1 $hexCs.$hexTn" --protocol-params-file protocol.json --out-file tx/close$1.draft`
else
    price=$(cat datum-$1.json | jq '.fields[1].fields[0].fields[1].int')
    fee=$(echo " $price * 0.02" | bc)
    fee=${fee%.*}

    if [ $fee -lt 2000000 ];
    then
        fee=2000000
    fi 
    owner1=$(cat wallets/owner1.addr) 
    owner2=$(cat wallets/owner2.addr)
    
    tx=`cardano-cli transaction build --alonzo-era $TESTNET --change-address ${FROM_WALLET_ADDRESS} --tx-in ${FROM_UTXO} --tx-in ${CON_FROM_UTXO} --tx-in-script-file auction.plutus --tx-in-datum-file datum-$1.json --tx-in-redeemer-file close.json --tx-in-collateral ${FROM_UTXO} --invalid-before $slot  --tx-out "${FROM_WALLET_ADDRESS} + 2000000 + 1 $hexCs.$hexTn" --tx-out "${FROM_WALLET_ADDRESS} + $price" --tx-out "$owner1 + $fee" --tx-out "$owner2 + $fee" --protocol-params-file protocol.json --out-file tx/close$1.draft`
fi 

cardano-cli transaction sign \
    --tx-body-file tx/close$1.draft \
    --signing-key-file wallets/${FROM_WALLET_NAME}.skey \
    $TESTNET \
    --out-file tx/close$1.signed

                                   
cardano-cli transaction submit --tx-file tx/close$1.signed $TESTNET

