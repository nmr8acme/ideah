import GHC
import HscTypes
import ErrUtils
import Bag
import Outputable
import MonadUtils
import System.Environment
import SrcLoc
import Data.Maybe
import FastString
import Control.Monad (unless)
import Data.Char (isUpper)
import Data.List (isPrefixOf, partition)
import System.Console.GetOpt
import System.FilePath.Posix (dropExtension)
import System.FilePath (takeBaseName)

locStr :: SrcLoc -> String
locStr loc = if isGoodSrcLoc loc then show (srcLocLine loc) ++ ":"
  ++ show (srcLocCol loc) else "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-"
  ++ locStr (srcSpanEnd span)

msgStr :: Message -> PrintUnqualified -> String
msgStr msg unqual = show $ msg (mkErrStyle unqual)

newMsgIndicator = "\f"

output1 :: (MonadIO m) => ErrMsg -> m ()
output1 msg = do
    let span   = head $ errMsgSpans msg
        unqual = errMsgContext msg
        printy = liftIO . putStrLn
        errMsg = msgStr (errMsgShortDoc msg) unqual
        isWarn = "Warning:" `isPrefixOf` errMsg
    printy newMsgIndicator
    printy $ fromMaybe "?" (fmap unpackFS $ srcSpanFileName_maybe span)
    printy $ (if isWarn then "W" else "E") ++ spanStr span
    printy $ msgStr (errMsgShortDoc msg) unqual
    printy $ msgStr (errMsgExtraInfo msg) unqual

outputBag :: (MonadIO m) => Bag ErrMsg -> m ()
outputBag msgs = do
    mapBagM output1 msgs
    return ()

output :: (MonadIO m) => SourceError -> m ()
output err = outputBag $ srcErrorMessages err

logger :: WarnErrLogger
logger Nothing = return ()
logger (Just err) = output err

catcher :: SourceError -> Ghc SuccessFlag
catcher err = do
    output err
    return Failed

doWalk :: [String] -> Bool -> [String] -> Ghc ()
doWalk cmdFlags skipOut files = do
    flg <- getSessionDynFlags
    (flg, _, _) <- parseDynamicFlags flg (map noLoc cmdFlags)
    setSessionDynFlags $ if skipOut  
       then flg { hscTarget = HscNothing, ghcLink = NoLink }
       else flg { ghcLink = NoLink }
    mapM_ (\file -> addTarget Target {
        targetId = TargetFile file Nothing
      , targetAllowObjCode = False
      , targetContents = Nothing })
      files
    loadWithLogger logger LoadAllTargets `gcatch` catcher
    warns <- getWarnings
    outputBag warns
    return ()

-- ./err_test
--    -g <path>           # ghc path
--    -o <path>           # output path
--    -s <path>           # source path
--    -c "<options>"      # compiler options
--    -e <file>           # source file to be made executable
--    <files>             # files to be compiled

data Options = Options
  { ghcPath         :: String
  , sourcePath      :: String
  , outputPath      :: String
  , compilerOptions :: [String]
  , exeFile         :: Maybe String
  } deriving Show

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['g'] ["ghcpath"]    (ReqArg (\path opt -> opt {ghcPath = path}) "DIR")
    "GHC path"
  , Option ['o'] ["outpath"]    (ReqArg (\path opt ->
    opt {outputPath = path}) "DIR") "output path"
  , Option ['s'] ["sourcepath"] (ReqArg (\path opt ->
    opt {sourcePath = path}) "DIR") "source path"
  , Option ['c'] ["ghcoptions"] (ReqArg (\opts opt ->
    opt {compilerOptions = words opts}) "STRING") "GHC options"
  , Option ['e'] ["toexe"]      (ReqArg (\file opt -> opt {exeFile = Just file}) "FILE")
    "file to be make executable"
  ]

defaultOpts :: Options
defaultOpts = Options
  { ghcPath         = ""
  , outputPath      = ""
  , sourcePath      = ""
  , compilerOptions = []
  , exeFile         = Nothing
  }

main = do
    args <- getArgs
    let (opts', files, errors)     = getOpt Permute options args
    unless (null errors) $ ioError $ userError $ concat errors
    let opts                      = foldl (\opt f -> f opt) defaultOpts opts'
        outPath                   = outputPath opts
        iOption                   = "-i" ++ outPath ++ ":" ++ sourcePath opts
        (modules, nonModules)     = partition (isUpper . head . takeBaseName) files
        skipOut = null outPath
        runGhcPath additionalOpts outputOpts = runGhc (Just (ghcPath opts))
          . doWalk (compilerOptions opts ++ additionalOpts ++
            (if skipOut then [] else outputOpts)) skipOut
    runGhcPath ["--make", "-c", iOption] ["-outputdir " ++ outPath] modules
    mapM_ (\file ->
        let ohiOption opt ext = opt ++ outPath ++ "/" ++ dropExtension file ++ ext
        in runGhcPath [ "-c", iOption]
                      [ohiOption "-o " ".o", ohiOption "-ohi " ".hi"] [file])
      nonModules
    case exeFile opts of
      Just toExe -> runGhcPath []
          (("-o " ++ outPath ++ "/" ++ dropExtension toExe ++ ".exe")
          : map ((++ ".o") . dropExtension) modules)
          [toExe]
      _          -> return ()
    return ()
