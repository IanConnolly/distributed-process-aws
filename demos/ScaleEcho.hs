{-# LANGUAGE TemplateHaskell #-}

import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Control.Monad (unless, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Distributed.Process (Process, expect)
import Control.Distributed.Process.Closure (remotable, mkClosure)
import Control.Distributed.Process.Backend.AWS

echoRemote :: () -> Backend -> Process ()
echoRemote () _backend = forever $ do
  str <- expect
  remoteSend (str :: String)

remotable ['echoRemote]

echoLocal :: LocalProcess ()
echoLocal = do
  str <- liftIO $ putStr "# " >> hFlush stdout >> getLine
  unless (null str) $ do
    localSend str
    liftIO $ putStr "Echo: " >> hFlush stdout
    echo <- localExpect
    liftIO $ putStrLn echo
    echoLocal

main :: IO ()
main = do
  args <- getArgs
  case args of
    "onvm":args' ->
      -- Pass execution to 'onVmMain' if we are running on the VM
      -- ('callOnVM' will provide the right arguments)
      onVmMain __remoteTable args'

    conf:user:port:_ -> do
      -- Initialize the AWS backend
      params <- defaultAWSParameters conf
      let params' = params { awsSshUserName = user }
      cf <- awsSetup $ awsConf params'
      cs <- createService cf "echoService" 1
      backend <- initializeBackend params' "echoService"
      -- Find the specified virtual machine
      vms <- findVMs backend
      let vm = head vms
      copyToVM backend vm

      -- Run the echo client proper
      callOnVM backend vm port $
        ProcessPair ($(mkClosure 'echoRemote) ())
                    echoLocal
