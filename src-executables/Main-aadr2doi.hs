{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

--import           Paths_aadr2doi                     (version)

import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
import           Control.Exception                  (catch, Exception)
import           Data.Version                       (showVersion, makeVersion)
import qualified Options.Applicative                as OP
import           System.Exit                        (exitFailure)
import           System.IO                          (hPutStrLn, stderr, stdout, hGetEncoding)
import Data.ByteString (breakSubstring)
import Data.ByteString.Lazy (toStrict)
import qualified Text.Regex.TDFA as R
import Data.ByteString (ByteString)
import Text.Regex.TDFA ((=~))
--import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as M


version = makeVersion [0,0,0]

-- data types
data AADR2DOIoptions = AADR2DOIoptions ByteString

data Options = CmdAADR2DOI AADR2DOIoptions

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

aadr2doiOptParser :: OP.Parser AADR2DOIoptions
aadr2doiOptParser = AADR2DOIoptions <$> optParsePaperKey

optParsePaperKey :: OP.Parser ByteString
optParsePaperKey = OP.strOption (
    OP.long "key" <> 
    OP.short 'k' <>
    OP.help "..."
    )


-----

data DOI = DOI String deriving Show

renderDOI :: DOI -> String
renderDOI (DOI x) = "https://doi.org" ++ x

--makeDOI :: ByteString -> DOI
--makeDOI x = DOI (B.unpack x)

runAADR2DOI :: AADR2DOIoptions -> IO ()
runAADR2DOI (AADR2DOIoptions toLookup) = do
    -- download html document
    hPutStrLn stderr "Downloading citation list"
    httpman <- H.newManager H.tlsManagerSettings
    let req = H.setQueryString [("q", Just "r")] "https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/index_v54.1.html"
    response <- H.httpLbs req httpman
    let responseBody = toStrict $ H.responseBody response
    -- extract paper paragraphs
    hPutStrLn stderr "Extracting individual papers"
    let (_,referenceSection) = breakSubstring "References:" responseBody
        separatorIndizes = map fst (R.getAllMatches (referenceSection =~ ("((\\.|<\\/h3>)(<br>|\\\n)+\\[)" :: ByteString)) :: [(Int, Int)])
        fromToIndizes = zip separatorIndizes (tail separatorIndizes ++ [B.length referenceSection])
        paperStrings = map (\(start,stop) -> B.take (stop - start) $ B.drop start referenceSection) fromToIndizes
    hPutStrLn stderr $ "Found " ++ show (length paperStrings) ++ " papers"
    -- extract paper keys and dois
    hPutStrLn stderr $ "Extracting paper keys and DOIs"
    let paperKeysRaw = map (\x -> x =~ ("\\[[^ ]+\\]" :: ByteString)) paperStrings :: [ByteString]
        paperKeys = map (removeFromStartAndEnd 1 1) paperKeysRaw
        paperDOIsRaw = map (\x -> x =~ ("(doi|DOI):[ ]?[^ ]+(\\. |<|$)" :: ByteString)) paperStrings :: [ByteString]
        -- see: https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
        paperDOIs = map (\x -> removeTrailingDotAndSpace $ x =~ ("10\\.[0-9]{4,}[^ \"/<>]*/[^ \"<>]+" :: ByteString)) paperDOIsRaw :: [ByteString]
        -- debugging:
        --let hu = zip3 paperKeys paperDOIsRaw paperDOIs
        --mapM_ (\x -> print x) hu
    -- define hashmap of valid papers
    hPutStrLn stderr $ "Removing papers with missing DOI"
    let presumablyValidPapers = filter (\(k,d) -> not (B.null k) && not (B.null d) ) $ zip paperKeys paperDOIs
    hPutStrLn stderr $ "Kept " ++ show (length presumablyValidPapers) ++ " papers"
    let papersHashMap = M.fromList presumablyValidPapers
    -- perform lookup
    hPutStrLn stderr $ "Performing DOI lookup for each requested key"
    case M.lookup toLookup papersHashMap of
        Nothing -> error "mist"
        Just x -> print x




removeFromStartAndEnd :: Int -> Int -> ByteString -> ByteString
removeFromStartAndEnd fromStart fromEnd xs = B.drop fromStart $ B.take (B.length xs - fromEnd) xs

removeTrailingDotAndSpace :: ByteString -> ByteString
removeTrailingDotAndSpace x1 = 
    let x2 = if B.take 2 (B.reverse x1) == ". " then removeFromStartAndEnd 0 2 x1 else x1
        x3 = if B.take 1 (B.reverse x2) == "."  then removeFromStartAndEnd 0 1 x2 else x2
    in x3