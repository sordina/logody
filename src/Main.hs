{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Prelude hiding (log)

import Data.Yaml
import GHC.Generics
import Control.Monad
import Data.Maybe
import Data.Either
import Data.Char
import System.IO
import System.Exit
import System.Environment
import Data.Map hiding (map, filter)
import Control.Concurrent.Async
import qualified System.Process as P
import qualified Control.Concurrent.Chan as C

data ProcessConfigItem = PCI
  { process :: Maybe String
  , shell   :: Maybe String
  , resume  :: Maybe [ String ]
  , args    :: Maybe [ String ]
  }
  deriving (Show, Generic)

instance FromJSON ProcessConfigItem

data Process = P
  { name       :: String
  , runner     :: Runner
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

decodeProcessConfig :: String -> IO ( Either ParseException (Map String ProcessConfigItem) )
decodeProcessConfig = decodeFileEither

decodeProcesses :: String -> IO ( Either [ParseException] [Process])
decodeProcesses fp = do
  pc <- mapLeft (:[]) <$> decodeProcessConfig fp
  return (pc >>= catEithers . map makeProcess . toAscList)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft  f (Left  a) = Left (f a)
mapLeft _f (Right b) = Right b

makeProcess :: (String, ProcessConfigItem) -> Either [ParseException] Process
makeProcess (_, PCI Nothing Nothing _ _)              = crash "specify process or a shell"
makeProcess (_, PCI (Just _) (Just _) _ _)            = crash "specify EITHER a process or a shell"
makeProcess (n, PCI (Just p) Nothing  r Nothing)      = P n (Program p []) <$> (makeResume r)
makeProcess (n, PCI (Just p) Nothing  r (Just a))     = P n (Program p a)  <$> (makeResume r)
makeProcess (n, PCI Nothing (Just s)  r Nothing)      = P n (Shell   s)    <$> (makeResume r)
makeProcess (n, PCI Nothing (Just s)  r (Just []))    = P n (Shell   s)    <$> (makeResume r)
makeProcess (_, PCI Nothing (Just _)  _ (Just (_:_))) = crash "shell commands do NOT take arguments"

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

help :: IO ()
help = do
  putStrLn "Usage: logdog CONFIG_FILE"
  putStrLn ""
  putStrLn "File Format Example:"
  putStrLn ""
  putStrLn "    ---"
  putStrLn "    foo:"
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
  putStrLn "      resume:"
  putStrLn "        - succeed"
  putStrLn "        - fail"

main :: IO ()
main = do
  args <- getArgs
  case args
    of []         -> help >> exitFailure
       ["-h"]     -> help
       ["--help"] -> help
       [conf]     -> do
         res <- decodeProcesses conf
         case res of Left  es -> print es
                     Right rs -> go rs

type ChanM a = C.Chan (Maybe a)

newLogChan :: IO (ChanM String)
newLogChan = C.newChan

getChanContents :: ChanM a -> IO [a]
getChanContents c = do
  cs <- C.getChanContents c
  return $ catMaybes $ takeWhile isJust cs

writeChan :: C.Chan (Maybe a) -> a -> IO ()
writeChan c a = C.writeChan c (Just a)

printLogs :: ChanM String -> IO ()
printLogs logs = getChanContents logs >>= mapM_ putStrLn

closeChan :: ChanM a -> IO ()
closeChan c = C.writeChan c Nothing

type Logger = Process -> String -> IO ()

go :: [Process] -> IO ()
go ps = do
  let namewidth = maximum (map (length . name) ps)
  logs <- newLogChan
  let logger p s = writeChan logs (name p ++ replicate (namewidth - length (name p)) ' ' ++ " | " ++ (filter isPrint s))
  a1 <- async $ mapConcurrently_ (run logger) ps
  a2 <- async $ printLogs logs
  wait a1
  closeChan logs
  wait a2

-- The main guy
--
run :: Logger -> Process -> IO ()
run log p =
  case runner p
    of Shell   s    -> log p ("Starting Process " ++ show p) >> kickoffShell      log p s
       Program s as -> log p ("Starting Process " ++ show p) >> kickoffProgram as log p s

kickoffShell :: Logger -> Process -> String -> IO ()
kickoffShell log p s = createProcess (P.shell s) >>= kickoff kickoffShell log p s

kickoffProgram :: [String] -> Logger -> Process -> String -> IO ()
kickoffProgram as log p s = createProcess (P.proc s as) >>= kickoff (kickoffProgram as) log p s

kickoff :: (Logger -> Process -> t -> IO ())
        -> Logger -> Process -> t
        -> (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
        -> IO ()
kickoff _ log p _ (_stdin, Nothing, _stderr, _pid) = log p "Failure -> Couldn't get STDOUT of Process"
kickoff f log p s (_stdin, Just stdout_h, _stderr, pid_h) = do
  untilM (hIsEOF stdout_h) $ do
    l <- hGetLine stdout_h
    log p ("stdout -> " ++ l)
  code <- P.waitForProcess pid_h
  case code
    of ExitSuccess -> do
        log p "Exited Successfully"
        when (succeed $ resumption $ p) $ do
          log p "Restarting process after success"
          (f log p s)
       ExitFailure c -> do
         log p ("Failure -> Failed with code " ++ show c)
         when (failure $ resumption $ p) $ do
           log p ("Restarting process after failure" ++ show c)
           f log p s

untilM :: Monad m => m Bool -> m () -> m ()
untilM c m = do
  b <- c
  when (not b) (m >> untilM c m)

createProcess :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
createProcess p = P.createProcess p { P.std_out = P.CreatePipe }
