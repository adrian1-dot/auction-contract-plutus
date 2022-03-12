


cardano-cli transaction build --tx-in 7f8a83bd41f443e2480dc7f7028438769d109b6b0ab0d8fe3f08e98e08a34683#1 --tx-in a4d9e20d32bb5418801f9123a8b441d4f83dfcd01e5f954a446944a98afbcd79#1 --tx-in abc42972cfa75d694014167d716a6b9e0593682b15b77071edad8556340566de#1 --tx-in dd8c04edb12ebb012a2208fd6d975bfc62a5fce0e74325685ecdfc40645d8b16#1 --tx-in 95b5ed39a6c3e13a78a58d94ecad56fdb6336f40bb99176057881d878bad3f9c#2 --tx-in 9af87852e81fddbd6611d8051a1afafce8d39548fdedf28603454b6c5a78ea26#2 --tx-out "$(cat wallets/wallet1.addr) + 5000000 + 1 2c6495a3987430afac1b2a84d3add628f1358b83cf288d5101e48e5e.7465737431" --tx-out "$(cat wallets/wallet1.addr) + 5000000 + 1 785a798ec27881fd9ab70b58feef37c917351e8b0e6b0f54bdbfce70.7465737432" --tx-out "$(cat wallets/wallet1.addr) + 5000000 + 1 d188eddb369c3f816eec97ec5bead852210060e8556ee0dcd9d10bcd.7465737433" --tx-out "$(cat wallets/wallet1.addr) + 5000000 + 1 c77eb2eaae6019d323a89805c17ad78db8cf0386905ab94b5969dd3e.7465737434" $TESTNET --out-file tx/mtx.draft --change-address $(cat wallets/wallet1.addr) --alonzo-era

cardano-cli transaction sign --tx-body-file tx/mtx.draft --signing-key-file wallets/wallet1.skey --out-file tx/mtx.signed 

cardano-cli transaction submit --tx-file tx/mtx.signed $TESTNET
