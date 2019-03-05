{- | Provides support for processing command-line arguments.
This is a simple wrapper around get-opt.
Here is an example of a typical usage:

> data Settings = Settings
>   { verbose :: Bool
>   , inPar   :: Int
>   , files   :: [String]
>   }
>
> options :: OptSpec Settings
> options = OptSpec
>   { progDefaults = Settings { verbose = False
>                             , inPar   = 1
>                             , files   = []
>                             }
>
>   , progOptions =
>       [ Option ['v'] ["verbose"]
>         "Display more information while working."
>         $ NoArg $ \s -> Right s { verbose = True }
>
>       , Option ['p'] ["par"]
>         "Process that many files at once."
>         $ ReqArg "NUM" $ \a s ->
>           case readMaybe a of
>             Just n | n > 0  -> Right s { inPar = n }
>             _               -> Left "Invalid value for `par`"
>       ]
>
>   , progParamDocs =
>       [ ("FILES",   "The files that need processing.") ]
>
>   , progParams = \p s -> Right s { files = p : files s }
>   }

Here is what the usage information looks like:

> *Main> dumpUsage options 
> Parameters:
>   FILES    The files that need processing.
>
> Flags:
>   -v      --verbose  Display more information while working.
>   -p NUM  --par=NUM  Process that many files at once.
-}

module SimpleGetOpt
  ( -- * Basic functionality
    getOpts
  , getOptsX
  , OptSpec(..)
  , OptDescr(..)
  , OptSetter
  , ArgDescr(..)
  , GetOptException(..)

  -- * Information and error reporting.
  , dumpUsage
  , reportUsageError
  , usageString

  -- * Direct interaction with GetOpt
  , specToGetOpt
  ) where

import qualified System.Console.GetOpt as GetOpt
import System.IO(stderr,hPutStrLn)
import System.Exit(exitFailure)
import System.Environment(getArgs)

import Control.Monad(unless)
import Control.Exception(Exception,throwIO,catch)



-- | Specification of a collection of options, described by type @a@.
data OptSpec a = OptSpec
  { progDefaults :: a
    -- ^ Default options.
    -- This is what is used if no other options are provided.

  , progOptions  :: [OptDescr a]
    -- ^ A list of options and command-line flags.

  , progParamDocs       :: [(String,String)]
    -- ^ Documentatio for the free-form parameters.

  , progParams      :: String -> OptSetter a
    -- ^ Add a parameter that is not an option or a flag
    -- (i.e., this is just a free form command line parameter).

  }

-- | Describe an option.
data OptDescr a = Option
  { optShortFlags  :: [Char]
  , optLongFlags   :: [String]
  , optDescription :: String
  , optArgument    :: ArgDescr a
  }

-- | Manipulate options of type @a@, with support for errors.
type OptSetter a = a -> Either String a

-- | Describe an option argumnet.
data ArgDescr a =

    NoArg (OptSetter a)
    -- ^ This option does not take an argument.

  | ReqArg String (String -> OptSetter a)
    -- ^ This optoin has a required arugment.
    -- The string describes the type of the argument.

  | OptArg String (Maybe String -> OptSetter a)
    -- ^ This optoin has an optional arugment.
    -- The string describes the type of the argument.


specToGetOpt :: OptSpec a -> [ GetOpt.OptDescr (OptSetter a) ]
specToGetOpt = map convertOpt . progOptions

convertArg :: ArgDescr a -> GetOpt.ArgDescr (OptSetter a)
convertArg arg =
  case arg of
    NoArg a    -> GetOpt.NoArg a
    ReqArg s a -> GetOpt.ReqArg a s
    OptArg s a -> GetOpt.OptArg a s

convertOpt :: OptDescr a -> GetOpt.OptDescr (OptSetter a)
convertOpt (Option a b c d) = GetOpt.Option a b (convertArg d) c



addOpt :: (a, [String]) -> (a -> Either String a) -> (a, [String])
addOpt (a,es) f = case f a of
                    Left e   -> (a,e:es)
                    Right a1 -> (a1,es)

addFile :: (String -> OptSetter a) -> (a, [String]) -> String -> (a,[String])
addFile add (a,es) file = case add file a of
                            Left e    -> (a,e:es)
                            Right a1  -> (a1,es)


-- | Get the command-line options and process them according to the given spec.
-- The options will be permuted to get flags.
-- Throws a 'GetOptException' if some problems are found.
getOptsX :: OptSpec a -> IO a
getOptsX os =
  do as <- getArgs
     let (funs,files,errs) = GetOpt.getOpt GetOpt.Permute (specToGetOpt os) as
     unless (null errs) $ throwIO (GetOptException errs)
     let (a, errs1) = foldl addOpt (progDefaults os,[]) funs
     unless (null errs1) $ throwIO (GetOptException errs1)
     let (b, errs2) = foldl (addFile (progParams os)) (a,[]) files
     unless (null errs2) $ throwIO (GetOptException errs2)
     return b


-- | Get the command-line options and process them according to the given spec.
-- The options will be permuted to get flags.
-- On failure, print an error message on standard error and exit.
getOpts :: OptSpec a -> IO a
getOpts os =
  getOptsX os `catch` \(GetOptException errs) -> reportUsageError os errs

-- | Print the given messages on 'stderr' and show the program's usage info,
-- then exit.
reportUsageError :: OptSpec a -> [String] -> IO b
reportUsageError os es =
  do hPutStrLn stderr "Invalid command line options:"
     hPutStrLn stderr $ unlines $ map ("  " ++) es
     dumpUsage os
     exitFailure

-- | Show the program's usage information on 'stderr'.
dumpUsage :: OptSpec a -> IO ()
dumpUsage os = hPutStrLn stderr (usageString os)


-- | A string descibing the options.
usageString :: OptSpec a -> String
usageString os = GetOpt.usageInfo (params ++ "Flags:") (specToGetOpt os)
  where
  params = case concatMap ppParam (progParamDocs os) of
             "" -> ""
             ps -> "Parameters:\n" ++ ps ++ "\n"

  ppParam (x,y) = "  " ++ x ++ "    " ++ y ++ "\n"

data GetOptException = GetOptException [String] deriving Show

instance Exception GetOptException




