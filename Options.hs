module Options
  ( Options(..)
  , defaultOptions
  , options
  , parseOptions
  ) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Options = Options
  { optVerbose :: Bool
  , optDecimalPlaces :: Int
  , optEpsilon :: Double
  , optDoPowerMethod :: Bool
  , optDoSVD :: Bool
  , optDoRecombine :: Bool
  , optTestTruncation :: Bool
  , optFilePath :: FilePath
  }

defaultOptions = Options
  { optVerbose = False
  , optDecimalPlaces = 3
  , optEpsilon = 1.25e-3
  , optDoPowerMethod = False
  , optDoSVD = False
  , optDoRecombine = False
  , optTestTruncation = False
  , optFilePath = ""
  }

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
  , Option "P" ["power-method"]
    (NoArg
      (\opt -> return opt { optDoPowerMethod = True }))
    "Compute the eigen-vectors and values of the 2D matrix"
  , Option "S" ["svd"]
    (NoArg
      (\opt -> return opt { optDoSVD = True }))
    "Compute the singular value decomposition of the 2D matrix"
  , Option "R" ["recombine"]
    (NoArg
      (\opt -> return opt { optDoRecombine = True }))
    "Compute the Frobenius norm of the difference between the 2D matrix and its reconstructions"
  , Option "T" ["test-truncation"]
    (NoArg
      (\opt -> return opt { optTestTruncation = True }))
    ("Compute the Frobenius norms of the differences between the 2D matrix and its reconstructions"
     ++ " from truncated singular values")
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
      filePath = if not (null nonOptions) then head nonOptions else "test.txt"
  in  foldl (>>=) (return defaultOptions { optFilePath = filePath }) actions
