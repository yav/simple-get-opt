import SimpleGetOpt
import Text.Read(readMaybe)

data Settings = Settings
  { verbose :: Bool
  , inPar   :: Int
  , files   :: [String]
  }


options :: OptSpec Settings
options = OptSpec
  { progDefaults = Settings { verbose = False
                            , inPar   = 1
                            , files   = []
                            }

  , progOptions =
      [ Option ['v'] ["verbose"]
        "Display more information while working."
        $ NoArg $ \s -> Right s { verbose = True }

      , Option ['p'] ["par"]
        "Process that many files at once."
        $ ReqArg "NUM" $ \a s ->
          case readMaybe a of
            Just n | n > 0  -> Right s { inPar = n }
            _               -> Left "Invalid value for `par`"
      ]

  , progParamDocs =
      [ ("FILES",   "The files that need processing.") ]

  , progParams = \p s -> Right s { files = p : files s }
  }



