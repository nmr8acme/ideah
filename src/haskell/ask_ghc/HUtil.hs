module HUtil where

import System.FilePath (equalFilePath)
import System.Directory (canonicalizePath)
import Control.Monad (filterM)
import System.Console.GetOpt
import Data.List (intercalate)

import GHC
import Outputable
import MonadUtils
import Parser
import Lexer
import StringBuffer
import FastString
import SrcLoc
import DynFlags (tracingDynFlags)

data Mode = Compile | CheckMain | GetIdType | GetDeclPos | ParseTree | FindUsages | Test | Help | AutoImport
    deriving (Show, Read, Enum, Bounded)

data Options = Options
    { mode            :: Mode
    , ghcPath         :: String
    , sourcePath      :: String
    , outputPath      :: String
    , compilerOptions :: [String]
    , position        :: (Int, Int)
    , moduleFile      :: FilePath
    , identifier      :: String
    }

mainFuncModeOption =
    let minMode :: Mode
        minMode = minBound
        maxMode :: Mode
        maxMode = maxBound
    in Option ['m'] ["main-func-mode"] (ReqArg (\mod opt  -> opt {mode = read mod}) "Mode")
            ("Compilation mode. Possible arguments:\n" ++ intercalate ", " (map show [minMode..maxMode]))
moduleOption       = Option ['f'] ["module"]         (ReqArg (\modf opt -> opt {moduleFile = modf}) "String") "Module for specified line and column numers"
ghcpathOption      = Option ['g'] ["ghcpath"]        (ReqArg (\path opt -> opt {ghcPath = path}) "DIR") "GHC lib path"
outpathOption      = Option ['o'] ["outpath"]        (ReqArg (\path opt -> opt {outputPath = path}) "DIR") "Output path"
sourcepathOption   = Option ['s'] ["sourcepath"]     (ReqArg (\path opt -> opt {sourcePath = path}) "DIR") "Source path"
ghcoptionsOption   = Option ['c'] ["ghcoptions"]     (ReqArg (\opts opt -> opt {compilerOptions = compilerOptions opt  ++ [opts]}) "String") "GHC options"
lineNumberOption   = Option ['l'] ["line-number"]    (ReqArg (\line opt -> opt {position = (read line, snd $ position opt)}) "Num") "Line number"
columnNumberOption = Option ['r'] ["column-number"]  (ReqArg (\col opt  -> opt {position = (fst $ position opt, read col)}) "Num") "Column number"
nameOption         = Option ['n'] ["identifier"]     (ReqArg (\name opt -> opt {identifier = name}) "String") "Name of unresolved identifier to be looked up in other modules"

defaultOpts :: Options
defaultOpts = Options
    { mode            = Help
    , ghcPath         = ""
    , outputPath      = ""
    , sourcePath      = ""
    , compilerOptions = []
    , position        = (0, 0)
    , moduleFile      = ""
    , identifier      = ""
    }

newMsgIndicator = "\f"

sdocToStringStyled :: PprStyle -> SDoc -> String
sdocToStringStyled style doc = show $ runSDoc doc (initSDocContext tracingDynFlags style)

sdocToString :: SDoc -> String
sdocToString doc = sdocToStringStyled defaultUserStyle doc

toString :: (Outputable a) => a -> String
toString x = sdocToString $ ppr x

unqualified x flgs = (sdocToStringStyled (defaultErrStyle flgs)) $ ppr x

setupFlags skipOut cmdFlags = do
    flg <- getSessionDynFlags
    (flg, _, _) <- parseDynamicFlags flg (map noLoc cmdFlags)
    setSessionDynFlags $ if skipOut
        then flg { hscTarget = HscNothing, ghcLink = NoLink }
        else flg

addTargetFile file = 
    addTarget Target { targetId           = TargetFile file Nothing
                     , targetAllowObjCode = False
                     , targetContents     = Nothing }

loadStdin = do
    str <- getContents
    return $ stringToStringBuffer str

-- todo: should be removed, use only loadStd    in
loadFile file = hGetStringBuffer file

loadHsFile :: FilePath -> Ghc ModSummary
loadHsFile file = do
    addTargetFile file
    load LoadAllTargets
    summaries <- depanal [] False
    absoluteFile <- liftIO $ canonicalizePath file
    mods <- filterM (moduleIsFile absoluteFile) summaries
    return $ if null mods then error $ "Cannot load module: " ++ file
                          else head mods
    where 
        moduleIsFile file modSum = liftIO $ case ml_hs_file $ ms_location modSum of
            (Just path) -> do
                absolutePath <- canonicalizePath path
                return $ equalFilePath file absolutePath
            Nothing -> return False

parseHsFile :: StringBuffer -> String -> Ghc (Either (SrcSpan, String) (Located (HsModule RdrName)))
parseHsFile buffer fileName = do
    flags <- getSessionDynFlags
    let loc = mkRealSrcLoc (mkFastString fileName) (lineToGhc 1) (colToGhc 1)
    let state = mkPState flags buffer loc
    let result = unP Parser.parseModule state
    case result of
        POk _ parsed -> return $ Right parsed
        PFailed loc msg -> return $ Left (loc, sdocToString msg)

lineToGhc :: Int -> Int
lineToGhc line = line

lineFromGhc :: Int -> Int
lineFromGhc line = line

colToGhc :: Int -> Int
colToGhc col = col

colFromGhc :: Int -> Int
colFromGhc col = col

realLocStr :: RealSrcLoc -> String
realLocStr loc = show (lineFromGhc $ srcLocLine loc) ++ ":" ++ show (colFromGhc $ srcLocCol loc) 

locStr :: SrcLoc -> String
locStr (RealSrcLoc loc) = realLocStr loc
locStr (UnhelpfulLoc _) = "?"

realLocFileName :: RealSrcLoc -> String
realLocFileName loc = unpackFS $ srcLocFile loc

locFileName :: SrcLoc -> String
locFileName (RealSrcLoc loc) = realLocFileName loc
locFileName (UnhelpfulLoc _) = "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-" ++ locStr (srcSpanEnd span)

isLoc :: SrcSpan -> Int -> Int -> Bool
isLoc (RealSrcSpan loc) ghcLine ghcCol = srcSpanStartLine loc == ghcLine && srcSpanStartCol loc == ghcCol
isLoc _ _ _ = False
