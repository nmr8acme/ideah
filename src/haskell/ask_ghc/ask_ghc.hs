module Main where

import Control.Monad
import System.Environment
import System.Console.GetOpt

import Compile
import CheckMain
import GetIdType
import GetDocu
import GetDeclPos

data Mode = Compile | CheckMain | GetIdType | GetDeclPos | GetDocu
    deriving Read

-- ./ask_ghc
--    -m                  # ask_ghc mode
--    -g <path>           # ghc lib path
--    -o <path>           # output path
--    -s <path>           # source path
--    -c "<options>"      # compiler options
--    -l <line>           # line number
--    -r <column>         # column number
--    <files>             # files to be compiled

data Options = Options
    { mode            :: Mode
    , ghcPath         :: String
    , sourcePath      :: String
    , outputPath      :: String
    , compilerOptions :: [String]
    , position        :: (Int, Int)
    , function        :: String
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['m'] ["main-func-mode"] (ReqArg (\mod opt  -> opt {mode = read mod}) "Mode") "Mode"
    , Option ['f'] ["function-docu"]  (ReqArg (\fun opt  -> opt {function = fun}) "Fun") "Function to retrieve documentation for"
    , Option ['g'] ["ghcpath"]        (ReqArg (\path opt -> opt {ghcPath = path}) "DIR") "GHC path"
    , Option ['o'] ["outpath"]        (ReqArg (\path opt -> opt {outputPath = path}) "DIR") "output path"
    , Option ['s'] ["sourcepath"]     (ReqArg (\path opt -> opt {sourcePath = path}) "DIR") "source path"
    , Option ['c'] ["ghcoptions"]     (ReqArg (\opts opt -> opt {compilerOptions = words opts}) "STRING") "GHC options"
    , Option ['l'] ["line-number"]    (ReqArg (\line opt -> opt {position = (read line, snd $ position opt)}) "Num") "line number"
    , Option ['r'] ["column-number"]  (ReqArg (\col opt  -> opt {position = (fst $ position opt, read col)}) "Num") "column number"
    ]

defaultOpts :: Options
defaultOpts = Options
    { mode            = Compile
    , ghcPath         = ""
    , outputPath      = ""
    , sourcePath      = ""
    , compilerOptions = []
    , position        = (0, 0)
    , function        = ""
    }

main = do
    args <- getArgs
    let (opts', files, errors)    = getOpt Permute options args
    unless (null errors) $ ioError $ userError $ concat errors
    let opts       = foldl (\opt f -> f opt) defaultOpts opts'
        ghcpath    = ghcPath opts
        srcpath    = sourcePath opts
        singleFile = head files
        pos        = position opts
    case mode opts of
         Compile    -> compile (outputPath opts) srcpath ghcpath (compilerOptions opts) files
         CheckMain  -> checkMain ghcpath singleFile
         GetIdType  -> getIdType srcpath ghcpath singleFile pos
         GetDeclPos -> getDeclPos srcpath ghcpath singleFile pos
         GetDocu    -> getDocu singleFile srcpath ghcpath pos
