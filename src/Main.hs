{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Yaml
import GHC.Generics
import Control.Monad
import Data.Maybe
import System.Exit
import Data.Either
import System.IO
import Data.Map hiding (map)
import Control.Concurrent.Async
import qualified System.Process as P
import qualified Control.Concurrent.Chan as C

data ProcessConfigItem = PCI
  { process :: Maybe String
  , shell   :: Maybe String
  , resume  :: [ String ]
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
  = Program String
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
makeProcess (_, PCI Nothing Nothing _  ) = crash "specify process or a shell"
makeProcess (_, PCI (Just _) (Just _) _) = crash "specify EITHER a process or a shell"
makeProcess (n, PCI (Just p) Nothing  r) = P n (Program p) <$> (makeResume r)
makeProcess (n, PCI Nothing (Just s)  r) = P n (Shell   s) <$> (makeResume r)

crash :: String -> Either [ParseException] b
crash s = Left [ AesonException s ]

makeResume :: [String] -> Either [ParseException] Resume
makeResume [] = return $ Resume False False
makeResume ("succeed": xs) = setSucceed <$> makeResume xs
makeResume ("fail"   : xs) = setFail    <$> makeResume xs
makeResume _  = crash "Resumption must only be one of succeed or fail"

setSucceed, setFail :: Resume -> Resume
setSucceed r = r { succeed = True }
setFail    r = r { failure = True }

catEithers :: Monoid a => [Either a b] -> Either a [b]
catEithers l =
  case partitionEithers l
  of ([], xs) -> Right xs
     (es, _ ) -> Left (mconcat es)

main :: IO ()
main = do
  res <- decodeProcesses "test/processes.yaml"
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
  logs <- newLogChan
  let logger p s = writeChan logs (name p ++ " | " ++ s)
  a1 <- async $ mapConcurrently_ (run logger) ps
  a2 <- async $ printLogs logs
  wait a1
  closeChan logs
  wait a2

-- The main guy
--
run :: Logger -> Process -> IO ()
run log p@(runner -> Shell   s) = log p ("Starting Process" ++ show p) >> kickoffShell   log p s
run log p@(runner -> Program s) = log p ("Starting Process" ++ show p) >> kickoffProgram log p s

kickoffShell   log p s = createProcess (P.shell   s) >>= kickoff kickoffShell   log p s
kickoffProgram log p s = createProcess (P.proc s []) >>= kickoff kickoffProgram log p s

kickoff f log p s (_stdin, Nothing, _stderr, _pid) = log p "Failure -> Couldn't get STDOUT of Process"
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
  when (not b) m

createProcess :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
createProcess p = P.createProcess p { P.std_out = P.CreatePipe }
