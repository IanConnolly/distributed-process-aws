{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import System.Random (randomIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Distributed.Process
  ( Process
  , NodeId
  , SendPort
  , newChan
  , sendChan
  , spawn
  , receiveChan
  , spawnLocal
  )
import Control.Distributed.Process.Backend.AWS
import Control.Distributed.Process.Closure
  ( remotable
  , remotableDecl
  , mkClosure
  )

randomElement :: [a] -> IO a
randomElement xs = do
  ix <- randomIO
  return (xs !! (ix `mod` length xs))

remotableDecl [
    [d| dfib :: ([NodeId], SendPort Integer, Integer) -> Process () ;
        dfib (_, reply, 0) = sendChan reply 0
        dfib (_, reply, 1) = sendChan reply 1
        dfib (nids, reply, n) = do
          nid1 <- liftIO $ randomElement nids
          nid2 <- liftIO $ randomElement nids
          (sport, rport) <- newChan
          _ <- spawn nid1 $ $(mkClosure 'dfib) (nids, sport, n - 2)
          _ <- spawn nid2 $ $(mkClosure 'dfib) (nids, sport, n - 1)
          n1 <- receiveChan rport
          n2 <- receiveChan rport
          sendChan reply $ n1 + n2
      |]
  ]

remoteFib :: ([NodeId], Integer) -> Backend -> Process ()
remoteFib (nids, n) _backend = do
  (sport, rport) <- newChan
  _ <- spawnLocal $ dfib (nids, sport, n)
  fib_n <- receiveChan rport
  mapM_ terminateNode nids
  remoteSend fib_n

remotable ['remoteFib]

printResult :: LocalProcess ()
printResult = do
  result <- localExpect :: LocalProcess Integer
  liftIO $ print result

main :: IO ()
main = do
  args <- getArgs
  case args of
    "onvm":args' -> onVmMain (__remoteTable . __remoteTableDecl) args'
    [conf, user, n] -> do
      params <- defaultAWSParameters conf
      let params' = params { awsSshUserName = user }
      cf <- awsSetup $ awsConf params'
      cs <- createService cf "fibService" 1
      backend <- initializeBackend params' "fibService"
      vms <- findVMs backend
      forM vms $ \vm -> copyToVM backend vm
      nids <- forM vms $ \vm -> spawnNodeOnVM backend vm "10080"
      callOnVM backend (head vms) "10081" $
        ProcessPair ($(mkClosure 'remoteFib) (nids, read n :: Integer))
                    printResult
    _ ->
      error "Invalid command line arguments"
