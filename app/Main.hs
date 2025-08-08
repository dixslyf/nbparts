module Main where

import Options.Applicative qualified as OA
import Unpack

newtype AppOptions = AppOptions
  { command :: Command
  }

newtype UnpackOptions = UnpackOptions
  { notebook :: FilePath
  }

newtype PackOptions = PackOptions
  { directory :: FilePath
  }

data Command = Unpack UnpackOptions | Pack PackOptions

unpackOptionsParser :: OA.Parser UnpackOptions
unpackOptionsParser = UnpackOptions <$> OA.argument OA.str (OA.metavar "NOTEBOOK" <> OA.help "Path to the notebook to unpack")

packOptionsParser :: OA.Parser PackOptions
packOptionsParser = PackOptions <$> OA.argument OA.str (OA.metavar "DIRECTORY" <> OA.help "Path to the directory to pack into a notebook")

commandParser :: OA.Parser Command
commandParser =
  OA.hsubparser $
    OA.command "unpack" (OA.info (Unpack <$> unpackOptionsParser) (OA.progDesc "Unpack a notebook"))
      <> OA.command "pack" (OA.info (Pack <$> packOptionsParser) (OA.progDesc "Pack a directory into a notebook"))

appOptionsParser :: OA.Parser AppOptions
appOptionsParser = AppOptions <$> commandParser

parseOpts :: IO AppOptions
parseOpts = OA.customExecParser prefs opts
  where
    prefs = OA.prefs OA.showHelpOnEmpty
    opts = OA.info (OA.helper <*> appOptionsParser) (OA.fullDesc <> OA.progDesc "Unpack a Jupyter notebook into its content, metadata and outputs")

main :: IO ()
main = do
  opts <- parseOpts
  case command opts of
    Unpack (UnpackOptions notebook) -> unpack notebook
    Pack (PackOptions directory) -> print directory
