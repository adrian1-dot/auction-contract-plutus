
function getInputTx() {
	BALANCE_FILE=/tmp/walletBalances.txt
	rm -f $BALANCE_FILE
	if [ -z "$1" ]
	then
		read -p 'Wallet Name: ' SELECTED_WALLET_NAME
	else
		echo 'Wallet Name: ' $1
		SELECTED_WALLET_NAME=$1
	fi
	./balance.sh $SELECTED_WALLET_NAME > $BALANCE_FILE
	SELECTED_WALLET_ADDR=$(cat ./wallets/$SELECTED_WALLET_NAME.addr)
	cat $BALANCE_FILE
	read -p 'TX row number: ' TMP
	TX_ROW_NUM="$(($TMP+2))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" $BALANCE_FILE)
	SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
}

