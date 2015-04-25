### distributed-process-aws

AWS EC2 backend for [Cloud Haskell](http://haskell-distributed.github.com)

Exposes roughly the same API as
[the Azure backend](https://github.com/haskell-distributed/distributed-process-azure).

Also exposes a number of new primitives that allow for the run-time creation of
new CloudServices and VirtualMachines and the scaling of said CloudServices.



### Installation

* Download and extract this repository.
* Initialize a cabal sandbox ```cabal-sandbox```
* Download the patched version of [aws-sdk](https://github.com/IanConnolly/aws-sdk-fork)
* Add the patched version of aws-sdk as a source to the sandbox.
* Download, extract, and add [aws-service-api](https://github.com/IanConnolly/aws-service-api) as a source to the sandbox.
* ```cabal install``` or ```cabal install -f build-demos```.


### Configuration

You'll need to generate an Amazon EC2 image with libssh2-1 installed.

Fill in the ```aws.config``` file with the relevant information.



### Outstanding issues before pushing to Hackage

* Need to sort something out with aws-sdk. Current version on Hackage (0.12.4) doesn't build. Presumably the newer versions on Github do. Ask someone to push them?
* Wrap some/all of the aws-service-api functions in the distributed-process-aws abstractions (specifically surrounding ```Backend```)
* Performance fixes (createVM in aws-service-api stands out right now)
