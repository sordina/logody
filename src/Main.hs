{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (log)

import Data.Yaml (ToJSON, FromJSON, ParseException(AesonException), decodeEither')
import GHC.Generics
import Control.Monad
import Data.Maybe
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Char8 (pack)
import Data.Either
import Data.Char
import System.IO
import System.Directory
import System.Exit
import System.Environment
import Data.Map (Map, toAscList)
import Control.Concurrent.Async
import qualified System.Process as P
import qualified Control.Concurrent.Chan as C

-- Types and Data

data ProcessConfigItem = PCI
  { process  :: Maybe String
  , shell    :: Maybe String
  , sanitise :: Maybe Bool
  , resume   :: Maybe [ String ]
  , args     :: Maybe [ String ]
  }
  deriving (Show, Generic)

data Process = P
  { name       :: String
  , runner     :: Runner
  , sane       :: Bool
  , resumption :: Resume
  }
  deriving (Show, Generic)

data Runner
  = Program String [String]
  | Shell   String
  deriving (Eq, Show, Generic)

data Resume = Resume
  { succeed :: Bool
  , failure :: Bool
  }
  deriving (Show, Generic)

type ChanM a = C.Chan (Maybe a)

type Logger = Process -> String -> IO ()

-- Instances

instance FromJSON ProcessConfigItem
instance ToJSON Process
instance ToJSON Runner
instance ToJSON Resume

-- Main

main :: IO ()
main = getArgs >>= processArguments

processArguments :: [String] -> IO ()
processArguments ("-h":_)             = help
processArguments ("--help":_)         = help
processArguments ("-v":_)             = version
processArguments ("--version":_)      = version
processArguments ("-n": ps)           = processConf ps ""
processArguments ("--no-config": ps)  = processConf ps ""
processArguments ("-f"     : "-": ps) = file "STDIN" >> getContents >>= processConf ps
processArguments ("--file" : "-": ps) = file "STDIN" >> getContents >>= processConf ps
processArguments ("-f"     : f  : ps) = file f       >> readFile f  >>= processConf ps
processArguments ("--file" : f  : ps) = file f       >> readFile f  >>= processConf ps
processArguments ps = do
  let conf = "logody.yaml"
  fe <- doesFileExist conf
  if fe
     then file conf    >> readFile conf >>= processConf ps
     else file "STDIN" >> getContents   >>= processConf ps

file :: String -> IO ()
file s = hPutStrLn stderr $ "Reading Configuration file " ++ s ++ "..."

processConf :: [String] -> String -> IO ()
processConf ps conf
    = decodeProcesses conf ps
  >>= either print sallyForth

test :: IO ()
test = readFile "./test/processes.yaml" >>= processConf []

version :: IO ()
version = putStrLn "0.2"

help :: IO ()
help = do
  putStrLn "Usage: logody [SHELL]* < CONFIG_FILE"
  putStrLn ""
  putStrLn "    echo -n | logody SHELL*"
  putStrLn "    or..."
  putStrLn "    logody [OPTIONS] [NAME]* < CONFIG_FILE"
  putStrLn ""
  putStrLn "Options:"
  putStrLn ""
  putStrLn " -h | --help       Print help and usage information."
  putStrLn " -v | --version    Print version information."
  putStrLn " -n | --no-config  Don't read any configuration, just accept shell string arguments."
  putStrLn " -f | --file       Specify config file. '-' for STDIN."
  putStrLn ""
  putStrLn "WARNING: logody will attempt to read logody.yaml"
  putStrLn "         failing that it will read from STDIN."
  putStrLn "         echo an empty string as input to skip configuration."
  putStrLn ""
  putStrLn "Config Format Example:"
  putStrLn ""
  putStrLn "    ---"
  putStrLn "    pluckProcesses:"
  putStrLn "      process: uname"
  putStrLn "      args:"
  putStrLn "        - \"-a\""
  putStrLn "    "
  putStrLn "    bar_:"
  putStrLn "      shell: \"echo bar && sleep 1 && exit 1\""
  putStrLn "      resume:"
  putStrLn "        - fail"
  putStrLn "    "
  putStrLn "    baz__:"
  putStrLn "      process: ./test/test.bash"
  putStrLn "      sanitise: false"
  putStrLn "      resume:"
  putStrLn "        - succeed"
  putStrLn "        - fail"

-- Config File Parsing

decodeProcessConfig :: String -> Either ParseException (Map String ProcessConfigItem)
decodeProcessConfig = decodeEither' . pack

makeShell :: Int -> String -> Process
makeShell i p = P ("process_" ++ show i) (Shell p) (True) (Resume False False)

decodeProcesses :: String -> [String] -> IO ( Either [ParseException] [Process])
decodeProcesses conf ps = do
  case (ps, conf)
    of ([], "") -> return $ crash "pass a config into STDIN, or specify shell arguments"
       (_,  "") -> return $ Right $ zipWith makeShell [0..] ps
       ([], _)  -> do
         let pc = mapLeft (:[]) $ decodeProcessConfig conf
         return (pc >>= catEithers . map makeProcess . toAscList)
       (_, _)  -> do
         let pc = mapLeft (:[]) $ decodeProcessConfig conf
         return (pc >>= pluckProcesses ps . catEithers . map makeProcess . toAscList)

pluckProcesses :: [String] -> Either a [Process] -> Either a [Process]
pluckProcesses ps = fmap (filter (flip elem ps . name))

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft  f (Left  a) = Left (f a)
mapLeft _f (Right b) = Right b

-- Process Construction

makeProcess :: (String, ProcessConfigItem) -> Either [ParseException] Process
makeProcess (_, PCI Nothing  Nothing  _ _ _)            = crash "specify process or a shell"
makeProcess (_, PCI (Just _) (Just _) _ _ _)            = crash "specify EITHER a process or a shell"
makeProcess (_, PCI Nothing  (Just _) _ _ (Just (_:_))) = crash "shell commands do NOT take arguments"
makeProcess (n, PCI (Just p) Nothing  c r Nothing)      = P n (Program p []) (sanity c) <$> (makeResume r)
makeProcess (n, PCI (Just p) Nothing  c r (Just a))     = P n (Program p a)  (sanity c) <$> (makeResume r)
makeProcess (n, PCI Nothing  (Just s) c r Nothing)      = P n (Shell   s)    (sanity c) <$> (makeResume r)
makeProcess (n, PCI Nothing  (Just s) c r (Just []))    = P n (Shell   s)    (sanity c) <$> (makeResume r)

sanity :: Maybe Bool -> Bool
sanity = fromMaybe True

crash :: String -> Either [ParseException] b
crash s = Left [ AesonException s ]

makeResume :: Maybe [String] -> Either [ParseException] Resume
makeResume Nothing = return $ Resume False False
makeResume (Just []) = return $ Resume False False
makeResume (Just ("succeed": xs)) = setSucceed <$> makeResume (Just xs)
makeResume (Just ("fail"   : xs)) = setFail    <$> makeResume (Just xs)
makeResume _  = crash "Resumption must only be one of succeed or fail"

setSucceed, setFail :: Resume -> Resume
setSucceed r = r { succeed = True }
setFail    r = r { failure = True }

catEithers :: Monoid a => [Either a b] -> Either a [b]
catEithers l =
  case partitionEithers l
  of ([], xs) -> Right xs
     (es, _ ) -> Left (mconcat es)

-- Logging

newLogChan :: IO (ChanM String)
newLogChan = C.newChan

getChanContents :: ChanM a -> IO [a]
getChanContents c = do
  cs <- C.getChanContents c
  return $ catMaybes $ takeWhile isJust cs

writeChan :: ChanM a -> a -> IO ()
writeChan c a = C.writeChan c (Just a)

printLogs :: ChanM String -> IO ()
printLogs logs = getChanContents logs >>= mapM_ putStrLn

closeChan :: ChanM a -> IO ()
closeChan c = C.writeChan c Nothing

makeLogger :: Int -> ChanM String -> Process -> String -> IO ()
makeLogger width logs p s = writeChan logs line
  where
  line    = name p ++ padding ++ " | " ++ if sane p then (filter isPrint $ noEscape s) else s
  padding = replicate (width - length (name p)) ' '

  -- STUPID - Should probably use a scheme rather than blocking yellow...
  noEscape ('\ESC' : '[' : 'H' : xs)             = noEscape xs
  noEscape ('\ESC' : '[' : '2' : 'J' : xs)       = noEscape xs
  noEscape ('\ESC' : '[' : '3' : '3' : 'm' : xs) = noEscape xs
  noEscape ('\ESC' : '[' : 'm' : xs)             = noEscape xs
  noEscape ('\ESC' : xs)                         = noEscape xs
  noEscape (x : xs)                              = x : noEscape xs
  noEscape []                                    = []

-- Process Inception and Running

sallyForth :: [Process] -> IO ()
sallyForth ps = do
  logs <- newLogChan
  let
    logger :: Process -> String -> IO ()
    logger = makeLogger (maximum (map (length . name) ps)) logs

  withAsync (mapConcurrently_ (embark logger) ps) $ \a1 -> do
    withAsync (printLogs logs) $ \a2 -> do
      wait a1
      closeChan logs
      wait a2

embark :: Logger -> Process -> IO ()
embark log p =
  case runner p
    of Shell   s    -> log p ("Starting Process " ++ unpack (encode p)) >> startShell      log p s
       Program s as -> log p ("Starting Process " ++ unpack (encode p)) >> startProgram as log p s

startShell :: Logger -> Process -> String -> IO ()
startShell log p s
    -- Use withCreateProcess in order to handle exceptions to theads cleanly
    = withCreateProcess (P.shell s) (\xi xo xe xp -> manage log p (xi, xo, xe, xp))
  >>= cleanup startShell log p s

startProgram :: [String] -> Logger -> Process -> String -> IO ()
startProgram as log p s
    = withCreateProcess (P.proc s as) (\xi xo xe xp -> manage log p (xi, xo, xe, xp))
  >>= cleanup (startProgram as) log p s

manage :: Logger -> Process
              -> (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
              -> IO (Maybe ExitCode)
manage log p (_stdin, Just stdout_h, Just stderr_h, pid_h) = do

  hSetBuffering stdout_h LineBuffering
  hSetBuffering stderr_h LineBuffering

  stdout_reader <- async $ untilM (hIsEOF stdout_h) $ do
    l <- hGetLine stdout_h
    log p ("stdout -> " ++ l)

  stderr_reader <- async $ untilM (hIsEOF stderr_h) $ do
    l <- hGetLine stderr_h
    log p ("stderr -> " ++ l)

  wait stdout_reader
  wait stderr_reader

  Just <$> P.waitForProcess pid_h

manage log p _ = do
  log p "Failure -> Couldn't get handles for process"
  return Nothing

cleanup :: (Logger -> Process -> t -> IO ())
        -> Logger
        -> Process -> t -> Maybe ExitCode
        -> IO ()
cleanup _ _ _ _ Nothing = return ()
cleanup f log p s (Just code) = case code
  of ExitSuccess -> do
      log p "Exited Successfully"
      when (succeed $ resumption $ p) $ do
        log p "Restarting process after success"
        (f log p s)

     ExitFailure c -> do
       log p ("Failure -> Failed with code " ++ show c)
       when (failure $ resumption $ p) $ do
         log p ("Restarting process after failure with exit code " ++ show c)
         f log p s

untilM :: Monad m => m Bool -> m () -> m ()
untilM c m = do
  b <- c
  when (not b) (m >> untilM c m)

createProcess :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
createProcess p =
  P.createProcess
    p { P.std_out = P.CreatePipe
      , P.std_err = P.CreatePipe
      }

withCreateProcess :: P.CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> P.ProcessHandle -> IO a) -> IO a
withCreateProcess p =
  P.withCreateProcess
    p { P.std_out = P.CreatePipe
      , P.std_err = P.CreatePipe
      }
