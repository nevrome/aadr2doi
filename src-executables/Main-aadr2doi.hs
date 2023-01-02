{-# LANGUAGE OverloadedStrings #-}

--import           Paths_aadr2doi                     (version)

import           Control.Exception                  (catch, Exception)
import           Data.Version                       (showVersion, makeVersion)
import qualified Options.Applicative                as OP
import           System.Exit                        (exitFailure)
import           System.IO                          (hPutStrLn, stderr, stdout, hGetEncoding)

version = makeVersion [0,0,0]

-- data types
data A2Doptions = A2Doptions Bool

data Options = CmdAADR2DOI A2Doptions

-- | Different exceptions for aadr2doi
data AADR2DOIException =
      TestException String -- ^ An exception to ...
    | TestException2 String -- ^ An exception to ...
    deriving (Show)

renderAADR2DOIException :: AADR2DOIException -> String 
renderAADR2DOIException (TestException s) = 
    "<!> Error: " ++ s
renderAADR2DOIException (TestException2 s) =
    "<!> Error: " ++ s

instance Exception AADR2DOIException

-- CLI interface configuration
main :: IO ()
main = do
    hPutStrLn stderr $ "aadr2doi v" ++ showVersion version
    -- prepare input parsing
    cmdOpts <- OP.customExecParser p optParserInfo
    catch (runCmd cmdOpts) handler
    where
        p = OP.prefs OP.showHelpOnEmpty
        handler :: AADR2DOIException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderAADR2DOIException e
            exitFailure

runCmd :: Options -> IO ()
runCmd o = case o of
    CmdAADR2DOI opts -> runAADR2DOI opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "..."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = CmdAADR2DOI <$> aadr2doiOptParser

aadr2doiOptParser :: OP.Parser A2Doptions
aadr2doiOptParser = A2Doptions <$> optParseQuiet

optParseQuiet :: OP.Parser Bool
optParseQuiet = OP.switch (
    OP.long "quiet" <> 
    OP.short 'q' <>
    OP.help "Suppress the printing of ..."
    )


-----

runAADR2DOI :: A2Doptions -> IO ()
runAADR2DOI _ = putStrLn "huhu"

