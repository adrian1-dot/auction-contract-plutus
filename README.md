# Auction Contract 

This project shows an auction contract.


## Test locally 

* run `cabal build`
* run `cabal repl`
* run `import Auction.Trace`
* now you can run a trace for every endpoint (bidTest, closeTest, startTest)

## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc)

Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc).


## Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc please do so over on the main [plutus
repository](https://github.com/input-output-hk/plutus).

For more interactive discussion, you can join the [IOG Technical Community
Discord](https://discord.gg/sSF5gmDBYg).

Thanks!
