{-# LANGUAGE UnicodeSyntax, LambdaCase, FlexibleContexts #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Lens
import Data.List (isPrefixOf)
import System.Environment
import System.Exit
import System.Process
import Data.List (intersperse)
import Text.Read (readMaybe)
import GHC.Exts (sortWith)

import qualified DMenu

runProc :: MonadIO m => String → [String] → String → m (Either String String)
runProc prog args sIn = liftIO $ do
  (exitCode, sOut, sErr) ←
    readCreateProcessWithExitCode (proc prog args) sIn
  pure $ case exitCode of
    ExitSuccess   → Right sOut
    ExitFailure _ → Left sErr

runProcOr :: MonadIO m => String → [String] → String → String → m String
runProcOr prog args sIn sDef = either (const sDef) id <$> runProc prog args sIn

data ProcInfo = ProcInfo
  { piUser        :: String
  , piPid         :: Integer
  , piCpuUsage    :: Double
  , piMemoryUsage :: Double
  , piVSZ         :: Integer
  , piRSS         :: Integer
  , piTTY         :: Maybe String
  , piStat        :: String
  , piStart       :: String
  , piTime        :: String
  , piCommand     :: String
  }

readProcInfo :: String → Maybe ProcInfo
readProcInfo s = case words s of
  user:pid:cpu:mem:vsz:rss:tty:stat:start:time:cmdWords
    | Just pid' ← readMaybe pid
    , Just cpu' ← readMaybe cpu
    , Just mem' ← readMaybe mem
    , Just vsz' ← readMaybe vsz
    , Just rss' ← readMaybe rss
    → let tty' | tty == "?" = Nothing
               | otherwise = Just tty
      in Just $ ProcInfo user pid' cpu' mem' vsz' rss' tty'
                         stat start time (take 100 $ unwords cmdWords)
                         -- FIXME: unwords . words loses whitespaces of command
  _ → Nothing

showProcInfos :: [ProcInfo] → [String]
showProcInfos pis = map f pairs
 where
  users = fillWithSP $ map piUser pis
  pids  = fillWithSPR $ map (show . piPid) pis
  cpus  = fillWithSPR $ map (show . piCpuUsage) pis
  mems  = fillWithSPR $ map (show . piMemoryUsage) pis
  cmds  = fillWithSP $ map piCommand pis
  pairs = zip users $ zip pids $ zip cpus $ zip mems cmds
  f (user,(pid,(cpu,(mem,cmd)))) =
    concat $ intersperse "  " [ pid, user, cpu ++ "% CPU", mem ++ "% MEM", cmd ]

getProcs :: MonadIO m => m [ProcInfo]
getProcs = do
  sOut ← runProcOr "ps" ["aux"] "" ""
  fmap concat $ forM (drop 1 $ lines sOut) $ \l → do
    case readProcInfo l of
      Nothing → pure []
      Just pi' → pure [pi']

fillWithSP :: [String] → [String]
fillWithSP ss = map f ss where
  f s = s ++ replicate (maxLength - length s) ' '
  maxLength = maximum (map length ss)

fillWithSPR :: [String] → [String]
fillWithSPR ss = map f ss where
  f s = replicate (maxLength - length s) ' ' ++ s
  maxLength = maximum (map length ss)

data ProcessOrder = CPU | MEM | PID

-- | Parse the command line arguments
readArgs
  :: [String] -- ^ Arguments from 'getArgs'
  -> IO ProcessOrder
readArgs args =
  execStateT (go $ words $ unwords args) PID
 where
  go []                          = pure ()
  go (a:as)
    | a == "--"                  = pure () -- All arguments after "--" are passed to dmenu later.
    | a == "-cpu"                = do put CPU; go as
    | a == "-mem"                = do put MEM; go as
    | a == "-pid"                = do put PID; go as
    | a `elem` ["-h", "--help"]  = liftIO $ do putStrLn usage; exitFailure
    | a == ""                    = go as
  go _                           = liftIO $ do putStrLn usage; exitFailure

main :: IO ()
main = do
  order ← readArgs =<< getArgs
  sortProcs ← case order of
    CPU → pure $ reverse . sortWith piCpuUsage
    MEM → pure $ reverse . sortWith piMemoryUsage
    PID → pure id
  procs ← sortProcs <$> getProcs
  let procItems = zip (map piPid procs) (showProcInfos procs)
  let cfg = do
        DMenu.prompt .= "kill -9"
        DMenu.forwardExtraArgs
  DMenu.selectWith cfg snd procItems >>= \case
    Right (pid,_) → callCommand $ "kill -9 " ++ show pid
    _             → pure ()

usage :: String
usage = unlines
  [ "USAGE"
  , "  dmenu-pkill [OPTIONS] [-- DMENUOPTIONS]"
  , ""
  , "  Get current processes with `ps aux`, optionally sort them by CPU or RAM"
  , "  usage, and ask via dmenu to kill one of the processes via `kill -9 <pid>`."
  , ""
  , "  All arguments, after the first `--` argument, are directly passed to dmenu."
  , ""
  , "OPTIONS"
  , "  -cpu"
  , "    Sort process list by CPU usage."
  , "  -mem"
  , "    Sort process list by memory usage."
  , "  -pid"
  , "    Sort process list by pid. (default)"
  , "  -h, --help"
  , "    Display this message."
  ]
