{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

import           Paths_aadr2doi          (version)

import           Control.Exception       (Exception, catch, throwIO)
import           Control.Exception.Base  (try)
import           Data.Bits               (Bits (xor))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BC8
import qualified Data.ByteString.Lazy    as BSL
import           Data.ByteString.UTF8    (fromString)
import qualified Data.HashMap.Strict     as M
import           Data.Version            (makeVersion, showVersion)
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
import qualified Options.Applicative     as OP
import           System.Exit             (exitFailure)
import           System.IO               (hGetEncoding, hPutStrLn, stderr,
                                          stdout)
import qualified Text.Regex.TDFA         as R
import           Text.Regex.TDFA         ((=~))

-- data types

data AADR2DOIException =
      WebAccessException H.HttpException
    | KeyNotThereException ByteString
    deriving (Show)

renderAADR2DOIException :: AADR2DOIException -> String
renderAADR2DOIException (KeyNotThereException s) =
    "Error: Paper key " ++ show s ++ " not available or no DOI on the AADR website"
renderAADR2DOIException (WebAccessException e) =
    "Error: Can't connect to the AADR website\n" ++ show e

instance Exception AADR2DOIException

newtype Options = CmdAADR2DOI AADR2DOIOptions

data AADR2DOIOptions = AADR2DOIOptions {
      _requested   :: DOIRequest
    , _doiShape    :: DOIShape
    , _aadrVersion :: String
    , _outFile     :: Maybe FilePath
}

data DOIRequest = ListAll | Keys [ByteString] | KeyFile FilePath

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

aadr2doiOptParser :: OP.Parser AADR2DOIOptions
aadr2doiOptParser = AADR2DOIOptions <$> optParseDOIRequest
                                    <*> optParseDOIOutShape
                                    <*> optParseAADRVersion
                                    <*> optParseOutFile

optParseDOIRequest :: OP.Parser DOIRequest
optParseDOIRequest = (Keys <$> optParsePaperKey) OP.<|> (KeyFile <$> optParseKeyFile) OP.<|> (optParseListAll *> pure ListAll)

optParsePaperKey :: OP.Parser [ByteString]
optParsePaperKey = OP.option (OP.eitherReader readKeysString) (
    OP.long "keys" <>
    OP.short 'k' <>
    OP.help "..."
    )
    where
        readKeysString :: String -> Either String [ByteString]
        readKeysString s = Right $ B.split 44 $ fromString s

optParseKeyFile :: OP.Parser FilePath
optParseKeyFile = OP.strOption (
    OP.long "inFile" <>
    OP.short 'i' <>
    OP.help "..."
    )

optParseListAll :: OP.Parser ()
optParseListAll = OP.flag' () (
    OP.long "list" <>
    OP.short 'l' <>
    OP.help "..."
    )

optParseDOIOutShape :: OP.Parser DOIShape
optParseDOIOutShape = OP.option (OP.eitherReader readDOIShape) (
    OP.long "doiShape" <>
    OP.help "..." <>
    OP.value Long
    )
    where
        readDOIShape :: String -> Either String DOIShape
        readDOIShape s = case s of
            "short" -> Right Short
            "long"  -> Right Long
            _       -> Left "must be short or long"


optParseAADRVersion :: OP.Parser String
optParseAADRVersion = OP.strOption (
    OP.long "aadrVersion" <>
    OP.help "One of: 54.1, 52.2, 50.0, 50.0, 44.3, 42.4" <>
    OP.value "54.1"
    )

optParseOutFile :: OP.Parser (Maybe FilePath)
optParseOutFile = OP.option (Just <$> OP.str) (
    OP.long "outFile" <>
    OP.short 'o' <>
    OP.value Nothing <>
    OP.showDefault
    )

-- program logic

newtype DOI = DOI ByteString deriving Show

data DOIShape = Short | Long

renderDOI :: DOIShape -> DOI -> ByteString
renderDOI Short (DOI x) = x
renderDOI Long (DOI x) = "https://doi.org/" <> x

runAADR2DOI :: AADR2DOIOptions -> IO ()
runAADR2DOI (AADR2DOIOptions toLookup doiShape aadrVersion outFile) = do
    -- download html document
    hPutStrLn stderr $ "Downloading citation list for AADR version " ++ aadrVersion
    httpman <- H.newManager H.tlsManagerSettings
    let request = H.parseRequest_ $ "https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/index_v" ++ aadrVersion ++ ".html"
    let req = H.setQueryString [("q", Just "r")] request
    eitherResponse <- (try $ H.httpLbs req httpman) :: IO (Either H.HttpException (H.Response BSL.ByteString))
    -- stop on error
    case eitherResponse of
        Left x -> throwIO $ WebAccessException x
        Right response -> do
            let responseBody = BSL.toStrict $ H.responseBody response
            -- extract paper paragraphs
            hPutStrLn stderr "Extracting individual papers"
            let (_,referenceSection) = B.breakSubstring "References:" responseBody
                separatorIndizes = map fst (R.getAllMatches (referenceSection =~ ("((\\.|<\\/h3>)(<br>|\\\n)+\\[)" :: ByteString)) :: [(Int, Int)])
                fromToIndizes = zip separatorIndizes (tail separatorIndizes ++ [B.length referenceSection])
                paperStrings = map (\(start,stop) -> B.take (stop - start) $ B.drop start referenceSection) fromToIndizes
            hPutStrLn stderr $ "Found " ++ show (length paperStrings) ++ " papers"
            -- extract paper keys and dois
            hPutStrLn stderr "Extracting paper keys and DOIs"
            let paperKeysRaw = map (\x -> x =~ ("\\[[^ ]+\\]" :: ByteString)) paperStrings :: [ByteString]
                paperKeys = map (removeFromStartAndEnd 1 1) paperKeysRaw
                paperDOIsRaw = map (\x -> x =~ ("(doi|DOI):[ ]?[^ ]+(\\. |<|$)" :: ByteString)) paperStrings :: [ByteString]
                -- about this regex: https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
                paperDOIs = map (\x -> DOI $ removeTrailingDotAndSpace $ x =~ ("10\\.[0-9]{4,}[^ \"/<>]*/[^ \"<>]+" :: ByteString)) paperDOIsRaw
                -- debugging:
                --let hu = zip3 paperKeys paperDOIsRaw paperDOIs
                --mapM_ (\x -> print x) hu
            -- define hashmap of valid papers
            hPutStrLn stderr "Removing papers with missing DOI"
            let presumablyValidPapers = filter (\(k,DOI d) -> not (B.null k) && not (B.null d) ) $ zip paperKeys paperDOIs
            hPutStrLn stderr $ "Kept " ++ show (length presumablyValidPapers) ++ " papers"
            let papersHashMap = M.fromList presumablyValidPapers
            -- prepare output
            case toLookup of
                ListAll -> do
                    hPutStrLn stderr "Preparing table output"
                    hPutStrLn stderr "---"
                    mapM_ (\(k,d) -> B.putStr $ k <> "\t" <> renderDOI doiShape d <> "\n") presumablyValidPapers
                Keys requestedKeys -> do
                    B.hPutStr stderr $ "Requested keys: " <> B.intercalate ", " requestedKeys <> "\n"
                    hPutStrLn stderr "Performing DOI lookup for each requested key"
                    hPutStrLn stderr "---"
                    mapM_ (performLookup papersHashMap) requestedKeys
                KeyFile p -> do
                    hPutStrLn stderr $ "Reading file " ++ p
                    inputFromFile <- B.readFile p
                    let requestedKeys = BC8.lines inputFromFile
                    B.hPutStr stderr $ "Requested keys: " <> B.intercalate ", " requestedKeys <> "\n"
                    hPutStrLn stderr "Performing DOI lookup for each requested key"
                    hPutStrLn stderr "---"
                    mapM_ (performLookup papersHashMap) requestedKeys
            where
                performLookup :: M.HashMap ByteString DOI -> ByteString -> IO ()
                performLookup papersHashMap x = case M.lookup x papersHashMap of
                    Nothing -> hPutStrLn stderr $ renderAADR2DOIException $ KeyNotThereException x
                    Just d  -> case outFile of
                        Nothing -> B.putStr $ renderDOI doiShape d <> "\n"
                        Just p  -> B.appendFile p $ renderDOI doiShape d <> "\n"

removeFromStartAndEnd :: Int -> Int -> ByteString -> ByteString
removeFromStartAndEnd fromStart fromEnd xs = B.drop fromStart $ B.take (B.length xs - fromEnd) xs

removeTrailingDotAndSpace :: ByteString -> ByteString
removeTrailingDotAndSpace x1 =
    let x2 = if B.take 2 (B.reverse x1) == ". " then removeFromStartAndEnd 0 2 x1 else x1
        x3 = if B.take 1 (B.reverse x2) == "."  then removeFromStartAndEnd 0 1 x2 else x2
    in x3
