module Main where

import Numeric.LinearAlgebra
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random

data Options = Options
  { optVerbose :: Bool
  , optDecimalPlaces :: Int
  , optEpsilon :: Double
  , optSingularVectorCount :: Int
  , optFilePath :: FilePath }

defaultOptions = Options
  { optVerbose = False
  , optDecimalPlaces = 3
  , optEpsilon = 1.25e-3
  , optSingularVectorCount = 5
  , optFilePath = "" }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "d" ["decimal-places"]
    (ReqArg
      (\arg opt -> return opt { optDecimalPlaces = read arg :: Int })
      "INT")
    "Number of decimal places to display"
  , Option "e" ["epsilon"]
    (ReqArg
      (\arg opt -> return opt { optEpsilon = read arg :: Double })
      "DOUBLE")
    "Precision parameter"
  , Option "n" ["singular-vector-count"]
    (ReqArg
      (\arg opt -> return opt { optSingularVectorCount = read arg :: Int })
      "INT")
    "Number of singular vectors to compute"
  , Option "v" ["verbose"]
    (NoArg
      (\opt -> return opt { optVerbose = True }))
    "Enable verbose messages"
  , Option "h" ["help"]
    (NoArg
      (\_ -> do prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
    "Show help"
  ]

parseOptions :: [String] -> IO Options
parseOptions args =
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  in  foldl (>>=) (return defaultOptions) actions

main :: IO ()
main = do
  -- Parse options
  opts <- parseOptions =<< getArgs
  let Options { optVerbose = verbose
              , optDecimalPlaces = decimalPlaces
              , optEpsilon = epsilon
              , optSingularVectorCount = singularVectorCount
              } = opts
      -- Setup matrix and vector display functins
      mDisp = putStr . disps decimalPlaces
      vDisp = putStr . vecdisp (disps decimalPlaces)
  -- Create a random number generator
  gen <- newStdGen
  putStrLn "Done."
