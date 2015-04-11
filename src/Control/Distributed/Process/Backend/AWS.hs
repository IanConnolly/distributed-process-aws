module Control.Distributed.Process.Backend.AWS
  ( -- * Initialization
    Backend(..)
  , AWSParameters(..)
  , defaultAWSParameters
  , initializeBackend
    -- * Utilities
  , findNamedVM
    -- * On-VM main
  , onVmMain
    -- * Re-exports from AWS Service Management
  , CloudService(..)
  , VirtualMachine(..)
  , Endpoint(..)
  , AWS.cloudServices
  , AWS.createService
  , AWS.scaleUpService
  , AWS.scaleDownService
  , AWS.addVM
  , AWS.destroyVM
    -- * Remote and local processes
  , ProcessPair(..)
  , RemoteProcess
  , LocalProcess
  , localSend
  , localExpect
  , remoteSend
    -- * High-level API
  , spawnNodeOnVM
  , terminateNode
  ) where

import Prelude hiding (catch)
import System.Environment (getEnv)
import System.FilePath ((</>), takeFileName)
import System.Environment.Executable (getExecutablePath)
import System.IO
  ( stdout
  , hFlush
  , hSetBinaryMode
  , stdin
  , stdout
  , stderr
  , Handle
  , hClose
  )
import qualified System.Posix.Process as Posix (forkProcess, createSession)
import Data.Maybe (listToMaybe)
import Data.Binary (Binary(get, put), encode, decode, getWord8, putWord8)
import Data.Digest.Pure.MD5 (md5, MD5Digest)
import qualified Data.ByteString as BSS
  ( ByteString
  , length
  , concat
  , hPut
  , hGet
  )
import qualified Data.ByteString.Char8 as BSSC (pack)
import qualified Data.ByteString.Lazy as BSL
  ( ByteString
  , readFile
  , length
  , fromChunks
  , toChunks
  , hPut
  , hGet
  )
import Data.Typeable (Typeable)
import Data.Foldable (forM_)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, when)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Control.Exception
  ( Exception
  , catches
  , Handler(Handler)
  , throwIO
  , SomeException
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)

-- SSH
import qualified Network.SSH.Client.LibSSH2 as SSH
  ( withSSH2
  , scpSendFile
  , withChannelBy
  , Session
  , readAllChannel
  , writeAllChannel
  , Channel
  )
import qualified Network.SSH.Client.LibSSH2.Foreign as SSH
  ( openChannelSession
  , channelExecute
  , writeChannel
  , readChannel
  , channelSendEOF
  )
import qualified Network.SSH.Client.LibSSH2.Errors as SSH
  ( ErrorCode
  , NULL_POINTER
  , getLastError
  )

-- CH
import Control.Distributed.Process
  ( Process
  , Closure
  , RemoteTable
  , catch
  , unClosure
  , ProcessId
  , getSelfPid
  , NodeId
  , processNodeId
  , register
  , expect
  , nsendRemote
  )
import Control.Distributed.Process.Serializable (Serializable)
import qualified Control.Distributed.Process.Internal.Types as CH
  ( LocalNode
  , LocalProcess(processQueue)
  , Message
  , payloadToMessage
  , messageToPayload
  , createMessage
  )
import Control.Distributed.Process.Node
  ( runProcess
  , forkProcess
  , newLocalNode
  , initRemoteTable
  )
import Control.Distributed.Process.Internal.CQueue (CQueue, enqueue)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport.Internal (encodeInt32, decodeInt32, prependLength)

-- Static
import Control.Distributed.Static
  ( Static
  , registerStatic
  , staticClosure
  , staticLabel
  )
import Data.Rank1Dynamic (toDynamic)

import Network.AWS.ServiceManagement
  ( CloudService(..)
  , VirtualMachine(..)
  , Endpoint(..)
  , awsConfig
  )
import qualified Network.AWS.ServiceManagement as AWS
  ( cloudServices
  , vmSshEndpoint
  , createService
  , scaleUpService
  , scaleDownService
  , addVM
  , destroyVM
  )


data Backend = Backend {
    findVMs :: IO [VirtualMachine]
  , copyToVM :: VirtualMachine -> IO ()
  , checkMD5 :: VirtualMachine -> IO Bool
  , callOnVM :: forall a. VirtualMachine -> String -> ProcessPair a -> IO a
  , spawnOnVM :: VirtualMachine -> String -> RemoteProcess () -> IO ProcessId
  } deriving (Typeable)

-- | AWS connection parameters
data AWSParameters = AWSParameters {
    awsSetup           :: FilePath
  , awsSshUserName     :: FilePath
  , awsSshPublicKey    :: FilePath
  , awsSshPrivateKey   :: FilePath
  , awsSshPassphrase   :: String
  , awsSshKnownHosts   :: FilePath
  , awsSshRemotePath   :: FilePath
  , awsSshLocalPath    :: FilePath
  }

instance Binary AWSParameters where
  put params = do
    put (awsSetup params)
    put (awsSshUserName params)
    put (awsSshPublicKey params)
    put (awsSshPrivateKey params)
    put (awsSshPassphrase params)
    put (awsSshKnownHosts params)
    put (awsSshRemotePath params)
    put (awsSshLocalPath params)
  get =
    AWSParameters <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

defaultAWSParameters :: FilePath  -- Path to aws.config
                     -> IO AWSParameters
defaultAWSParameters conf = do
  home  <- getEnv "HOME"
  user  <- getEnv "USER"
  self  <- getExecutablePath
  return AWSParameters
    { awsSetup         = conf
    , awsSshUserName   = user
    , awsSshPublicKey  = home </> ".ssh" </> "id_rsa.pub"
    , awsSshPrivateKey = home </> ".ssh" </> "id_rsa"
    , awsSshPassphrase = ""
    , awsSshKnownHosts = home </> ".ssh" </> "known_hosts"
    , awsSshRemotePath = takeFileName self
    , awsSshLocalPath  = self
    }
-- | Initialize the backend
initializeBackend :: AWSParameters
                  -> String
                  -> IO Backend
initializeBackend params cloudService =
  return Backend {
      findVMs   = apiFindVMs params cloudService
    , copyToVM  = apiCopyToVM params
    , checkMD5  = apiCheckMD5 params
    , callOnVM  = apiCallOnVM params cloudService
    , spawnOnVM = apiSpawnOnVM params cloudService
    }

-- | Find virtual machines
apiFindVMs :: AWSParameters -> String -> IO [VirtualMachine]
apiFindVMs params cloudService = do
  config <- awsConfig $ awsSetup params
  css <- AWS.cloudServices config
  case filter ((== cloudService) . cloudServiceName) css of
    [cs] -> return $ cloudServiceVMs cs
    _    -> return []

-- | Start a CH node on the given virtual machine
apiCopyToVM :: AWSParameters -> VirtualMachine -> IO ()
apiCopyToVM params vm =
  void . withSSH2 params vm $ \s -> catchSshError s $ do
    SSH.scpSendFile s 0o700 (awsSshLocalPath params) (awsSshRemotePath params)
    SSH.scpSendFile s 0o700 (awsSetup params) (takeFileName $ awsSetup params)

-- | Call a process on a VM
apiCallOnVM :: AWSParameters
            -> String
            -> VirtualMachine
            -> String
            -> ProcessPair a
            -> IO a
apiCallOnVM = runOnVM False

apiSpawnOnVM :: AWSParameters
             -> String
             -> VirtualMachine
             -> String
             -> Closure (Backend -> Process ())
             -> IO ProcessId
apiSpawnOnVM params cloudService vm port rproc =
  runOnVM True params cloudService vm port $
    ProcessPair rproc localExpect

-- | Internal generalization of 'spawnOnVM' and 'callOnVM'
runOnVM :: Bool
        -> AWSParameters
        -> String
        -> VirtualMachine
        -> String
        -> ProcessPair a
        -> IO a
runOnVM bg params cloudService vm port ppair =
  withSSH2 params vm $ \s -> do
    let exe = "PATH=. " ++ awsSshRemotePath params
           ++ " onvm"
           ++ " " ++ vmIpAddress vm
           ++ " " ++ port
           ++ " " ++ cloudService
           ++ " " ++ show bg
           ++ " 2>&1"
    let paramsEnc = encode params
    let rprocEnc  = encode (ppairRemote ppair)
    (status, r) <- SSH.withChannelBy (SSH.openChannelSession s) id $ \ch -> do
      SSH.channelExecute ch exe
      SSH.writeChannel ch (encodeInt32 (BSL.length rprocEnc))
      SSH.writeAllChannel ch rprocEnc
      SSH.writeChannel ch (encodeInt32 (BSL.length paramsEnc))
      SSH.writeAllChannel ch paramsEnc
      runLocalProcess (ppairLocal ppair) ch
    if status == 0
      then return r
      else error "runOnVM: Non-zero exit status" -- This would a bug

-- | Check the MD5 hash of the executable on the remote machine
apiCheckMD5 :: AWSParameters -> VirtualMachine -> IO Bool
apiCheckMD5 params vm = do
  hash <- localHash params
  withSSH2 params vm $ \s -> do
    (r, _) <- SSH.withChannelBy (SSH.openChannelSession s) id $ \ch -> do
      SSH.channelExecute ch "md5sum -c --status"
      SSH.writeChannel ch . BSSC.pack $ show hash ++ "  " ++ awsSshRemotePath params
      SSH.channelSendEOF ch
      SSH.readAllChannel ch
    return (r == 0)

withSSH2 :: AWSParameters -> VirtualMachine -> (SSH.Session -> IO a) -> IO a
withSSH2 params (AWS.vmSshEndpoint -> Just ep) =
  SSH.withSSH2 (awsSshKnownHosts params)
               (awsSshPublicKey params)
               (awsSshPrivateKey params)
               (awsSshPassphrase params)
               (awsSshUserName params)
               (endpointVip ep)
               (read $ endpointPort ep)
withSSH2 _ vm =
  error $ "withSSH2: No SSH endpoint for virtual machine " ++ vmName vm

catchSshError :: SSH.Session -> IO a -> IO a
catchSshError s io =
    catches io [ Handler handleErrorCode
               , Handler handleNullPointer
               ]
  where
    handleErrorCode :: SSH.ErrorCode -> IO a
    handleErrorCode _ = do
      (_, str) <- SSH.getLastError s
      error str

    handleNullPointer :: SSH.NULL_POINTER -> IO a
    handleNullPointer _ = do
      (_, str) <- SSH.getLastError s
      error str

localHash :: AWSParameters -> IO MD5Digest
localHash params = md5 <$> BSL.readFile (awsSshLocalPath params)

findNamedVM :: Backend -> String -> IO (Maybe VirtualMachine)
findNamedVM backend vm =
  listToMaybe . filter ((== vm) . vmName) <$> findVMs backend

data ProcessPair a = ProcessPair {
    ppairRemote :: RemoteProcess ()
  , ppairLocal  :: LocalProcess a
  }

type RemoteProcess a = Closure (Backend -> Process a)

newtype LocalProcess a = LocalProcess { unLocalProcess :: ReaderT SSH.Channel IO a }
  deriving (Functor, Monad, MonadIO, MonadReader SSH.Channel)

runLocalProcess :: LocalProcess a -> SSH.Channel -> IO a
runLocalProcess = runReaderT . unLocalProcess

localSend :: Serializable a => a -> LocalProcess ()
localSend x = LocalProcess $ do
  ch <- ask
  liftIO $ mapM_ (SSH.writeChannel ch)
         . prependLength
         . CH.messageToPayload
         . CH.createMessage
         $ x

localExpect :: Serializable a => LocalProcess a
localExpect = LocalProcess $ do
  ch <- ask
  liftIO $ do
    isE <- readIntChannel ch
    len <- readIntChannel ch
    lenAgain <- readIntChannel ch
    when (len /= lenAgain) $ throwIO (userError "Internal error: protocol violation (perhaps the remote binary is not installed correctly?)")
    msg <- readSizeChannel ch len
    if isE /= 0
      then error (decode msg)
      else return (decode msg)

remoteSend :: Serializable a => a -> Process ()
remoteSend = liftIO . remoteSend'

remoteSend' :: Serializable a => a -> IO ()
remoteSend' = remoteSendFlagged 0

remoteThrow :: Exception e => e -> IO ()
remoteThrow e = remoteSendFlagged 1 (show e) >> throwIO e

remoteSendFlagged :: Serializable a => Int -> a -> IO ()
remoteSendFlagged flags x = do
  let enc = encode x
  BSS.hPut stdout (encodeInt32 flags)
  -- See 'localExpect' for why we send the length twice
  BSS.hPut stdout (encodeInt32 (BSL.length enc))
  BSS.hPut stdout (encodeInt32 (BSL.length enc))
  BSL.hPut stdout enc
  hFlush stdout

onVmMain :: (RemoteTable -> RemoteTable) -> [String] -> IO ()
onVmMain rtable [host, port, cloudService, bg] = do
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    Just rprocEnc  <- getWithLength stdin
    Just paramsEnc <- getWithLength stdin
    backend <- initializeBackend (decode paramsEnc) cloudService
    let rproc = decode rprocEnc
    lprocMVar <- newEmptyMVar :: IO (MVar CH.LocalProcess)
    if read bg
      then
        void . Posix.forkProcess $ do
          -- We inherit the file descriptors from the parent, so the SSH
          -- session will not be terminated until we close them
          void Posix.createSession
          startCH rproc lprocMVar backend
            (\node proc -> runProcess node $ do
              us <- getSelfPid
              liftIO $ do
                remoteSend' us
                mapM_ hClose [stdin, stdout, stderr]
              proc)
      else do
        startCH rproc lprocMVar backend forkProcess
        lproc <- readMVar lprocMVar
        queueFromHandle stdin (CH.processQueue lproc)
  where
    startCH :: RemoteProcess ()
            -> MVar CH.LocalProcess
            -> Backend
            -> (CH.LocalNode -> Process () -> IO a)
            -> IO ()
    startCH rproc lprocMVar backend go = do
      mTransport <- createTransport host port defaultTCPParameters
      case mTransport of
        Left err -> remoteThrow err
        Right transport -> do
          node <- newLocalNode transport (rtable . __remoteTable $ initRemoteTable)
          void . go node $ do
            ask >>= liftIO . putMVar lprocMVar
            proc <- unClosure rproc :: Process (Backend -> Process ())
            catch (proc backend)
                  (liftIO . (remoteThrow :: SomeException -> IO ()))
onVmMain _ _
  = error "Invalid arguments passed on onVmMain"

-- | Read a 4-byte length @l@ and then an @l@-byte payload
--
-- Returns Nothing on EOF
getWithLength :: Handle -> IO (Maybe BSL.ByteString)
getWithLength h = do
  lenEnc <- BSS.hGet h 4
  if BSS.length lenEnc < 4
    then return Nothing
    else do
      let len = decodeInt32 lenEnc
      bs <- BSL.hGet h len
      if BSL.length bs < fromIntegral len
        then return Nothing
        else return (Just bs)

queueFromHandle :: Handle -> CQueue CH.Message -> IO ()
queueFromHandle h q = do
  mPayload <- getWithLength stdin
  forM_ mPayload $ \payload -> do
    enqueue q $ CH.payloadToMessage (BSL.toChunks payload)
    queueFromHandle h q

readSizeChannel :: SSH.Channel -> Int -> IO BSL.ByteString
readSizeChannel ch = go []
  where
    go :: [BSS.ByteString] -> Int -> IO BSL.ByteString
    go acc 0    = return (BSL.fromChunks $ reverse acc)
    go acc size = do
      bs <- SSH.readChannel ch (fromIntegral (0x400 `min` size))
      go (bs : acc) (size - BSS.length bs)

readIntChannel :: SSH.Channel -> IO Int
readIntChannel ch =
  decodeInt32 . BSS.concat . BSL.toChunks <$> readSizeChannel ch 4

--------------------------------------------------------------------------------
-- High-level API                                                             --
--------------------------------------------------------------------------------

data ServiceProcessMsg =
    ServiceProcessTerminate
  deriving Typeable

instance Binary ServiceProcessMsg where
  put ServiceProcessTerminate = putWord8 0
  get = do
    header <- getWord8
    case header of
      0 -> return ServiceProcessTerminate
      _ -> fail "ServiceProcessMsg.get"

serviceProcess :: Backend -> Process ()
serviceProcess _backend = do
    us <- getSelfPid
    register "$awsBackendServiceProcess" us
    go
  where
    go = do
      msg <- expect
      case msg of
        ServiceProcessTerminate ->
          return ()

serviceProcessStatic :: Static (Backend -> Process ())
serviceProcessStatic = staticLabel "serviceProcess"

-- | Start a new Cloud Haskell node on the given virtual machine
spawnNodeOnVM :: Backend -> VirtualMachine -> String -> IO NodeId
spawnNodeOnVM backend vm port =
  processNodeId <$> spawnOnVM backend vm port (staticClosure serviceProcessStatic)

-- | Terminate a node started with 'spawnNodeOnVM'
terminateNode :: NodeId -> Process ()
terminateNode nid = nsendRemote nid "$awsBackendServiceProcess" ServiceProcessTerminate

__remoteTable :: RemoteTable -> RemoteTable
__remoteTable = registerStatic "serviceProcess" (toDynamic serviceProcess)
