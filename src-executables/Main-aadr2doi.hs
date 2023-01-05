{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

--import           Paths_aadr2doi                     (version)

import           Control.Exception       (Exception, catch, throwIO)
import           Control.Exception.Base  (try)
import           Data.Bits               (Bits (xor))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.HashMap.Strict     as M
import           Data.Version            (makeVersion, showVersion)
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
import qualified Options.Applicative     as OP
import           System.Exit             (exitFailure)
import           System.IO               (hGetEncoding, hPutStrLn, stderr,
                                          stdout)
import           Text.Regex.TDFA         ((=~))
import qualified Text.Regex.TDFA         as R
import Data.ByteString.UTF8 (fromString)

version = makeVersion [0,0,0]

-- data types

-- different exceptions for aadr2doi
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

data Options = CmdAADR2DOI AADR2DOIOptions

data AADR2DOIOptions = AADR2DOIOptions {
      _requested :: DOIRequest
    , _aadrVersion :: String
}

data DOIRequest = Keys [ByteString] | ListAll

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
                                    <*> optAADRVersion

optParseDOIRequest :: OP.Parser DOIRequest
optParseDOIRequest = (Keys <$> optParsePaperKey) OP.<|> (optParseListAll *> pure ListAll)

optParsePaperKey :: OP.Parser [ByteString]
optParsePaperKey = OP.option (OP.eitherReader readKeysString) (
    OP.long "keys" <>
    OP.short 'k' <>
    OP.help "..."
    )
    where
        readKeysString :: String -> Either String [ByteString]
        readKeysString s = Right $ B.split 44 $ fromString s

optParseListAll :: OP.Parser ()
optParseListAll = OP.flag' () (
    OP.long "list" <>
    OP.short 'l' <>
    OP.help "..."
    )

optAADRVersion :: OP.Parser String
optAADRVersion = OP.strOption (
    OP.long "aadrVersion" <>
    OP.help "One of: 54.1, 52.2, 50.0, 50.0, 44.3, 42.4" <>
    OP.value "54.1"
    )

-----

newtype DOI = DOI ByteString deriving Show

renderLongDOI :: DOI -> ByteString
renderLongDOI (DOI x) = "https://doi.org/" <> x

renderShortDOI :: DOI -> ByteString
renderShortDOI (DOI x) = x

--makeDOI :: ByteString -> DOI
--makeDOI x = DOI (B.unpack x)

runAADR2DOI :: AADR2DOIOptions -> IO ()
runAADR2DOI (AADR2DOIOptions toLookup aadrVersion) = do
    -- download html document
    hPutStrLn stderr $ "Downloading citation list for AADR version " ++ aadrVersion
    httpman <- H.newManager H.tlsManagerSettings
    let request = H.parseRequest_ $ "https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/index_v" ++ aadrVersion ++ ".html"
    let req = H.setQueryString [("q", Just "r")] request
    eitherResponse <- (try $ H.httpLbs req httpman) :: IO (Either H.HttpException (H.Response BSL.ByteString))
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
                -- see: https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
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
                    print papersHashMap
                Keys xs -> do
                    -- perform lookup
                    hPutStrLn stderr "Performing DOI lookup for each requested key"
                    hPutStrLn stderr "---"
                    mapM_ performLookup xs 
                    where
                        performLookup :: ByteString -> IO ()
                        performLookup x = case M.lookup x papersHashMap of
                            Nothing -> throwIO $ KeyNotThereException x
                            Just x  -> B.putStr $ renderLongDOI x <> "\n"
                    




removeFromStartAndEnd :: Int -> Int -> ByteString -> ByteString
removeFromStartAndEnd fromStart fromEnd xs = B.drop fromStart $ B.take (B.length xs - fromEnd) xs

removeTrailingDotAndSpace :: ByteString -> ByteString
removeTrailingDotAndSpace x1 =
    let x2 = if B.take 2 (B.reverse x1) == ". " then removeFromStartAndEnd 0 2 x1 else x1
        x3 = if B.take 1 (B.reverse x2) == "."  then removeFromStartAndEnd 0 1 x2 else x2
    in x3
