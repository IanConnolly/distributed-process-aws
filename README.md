### distributed-process-aws

AWS EC2 backend for [Cloud Haskell](http://haskell-distributed.github.com)

Will exposed roughly the same API as
[the Azure backend](https://github.com/haskell-distributed/distributed-process-azure).

Will be used as a basis for exposing Cloud Haskell 'wrapped' AWS EC2 primitives in a second iteration
(or possibly another library). The intent is to give the programmer the ability to easily create and destroy
_VMs_ on the fly, as easily as Cloud Haskell itself manages the lifecycle of processes.

