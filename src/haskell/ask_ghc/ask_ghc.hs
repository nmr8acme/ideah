module Main where

import Control.Monad
import System.Environment
import System.Console.GetOpt
import Data.List (intersperse)

import HUtil
import Compile
import CheckMain
import GetIdType
import GetDeclPos
import ParseTree
import FindUsages

-- ./ask_ghc
--    -m                  # ask_ghc mode
--    -f                  # module for specified line and column numbers
--    -g <path>           # ghc lib path
--    -o <path>           # output path
--    -s <path>           # source path
--    -c "<options>"      # compiler options
--    -l <line>           # line number
--    -r <column>         # column number
--    <files>             # files to be compiled

options :: [OptDescr (Options -> Options)]
options =
    let minMode :: Mode
        minMode = minBound
        maxMode :: Mode
        maxMode = maxBound
    in  [ Option ['m'] ["main-func-mode"] (ReqArg (\mod opt  -> opt {mode = read mod}) "Mode")
            ("Compilation mode. Possible arguments:\n" ++ (concat $ intersperse ", " $ map show [minMode..maxMode]))
        , Option ['f'] ["module"]         (ReqArg (\modf opt -> opt {moduleFile = modf}) "String") "Module for specified line and column numers"
        , Option ['g'] ["ghcpath"]        (ReqArg (\path opt -> opt {ghcPath = path}) "DIR") "GHC lib path"
        , Option ['o'] ["outpath"]        (ReqArg (\path opt -> opt {outputPath = path}) "DIR") "Output path"
        , Option ['s'] ["sourcepath"]     (ReqArg (\path opt -> opt {sourcePath = path}) "DIR") "Source path"
        , Option ['c'] ["ghcoptions"]     (ReqArg (\opts opt -> opt {compilerOptions = words opts}) "String") "GHC options"
        , Option ['l'] ["line-number"]    (ReqArg (\line opt -> opt {position = (read line, snd $ position opt)}) "Num") "Line number"
        , Option ['r'] ["column-number"]  (ReqArg (\col opt  -> opt {position = (fst $ position opt, read col)}) "Num") "Column number"
        ]

main = do
    args <- getArgs
    let (opts', files, errors) = getOpt Permute options args
    unless (null errors) $ ioError $ userError $ concat errors
    let opts       = foldl (\opt f -> f opt) defaultOpts opts'
        ghcpath    = ghcPath opts
        srcpath    = sourcePath opts
        singleFile = head files
        pos        = position opts
        compOpts   = compilerOptions opts
    case mode opts of
        Help       -> putStrLn $ usageInfo "Usage: ask_ghc [OPTION...] files...\n" options
        Compile    -> compile (outputPath opts) srcpath ghcpath compOpts files
        CheckMain  -> checkMain compOpts ghcpath singleFile
        GetIdType  -> getIdType compOpts srcpath ghcpath singleFile pos
        GetDeclPos -> getDeclPos compOpts srcpath ghcpath singleFile pos
        ParseTree  -> parseTree compOpts ghcpath singleFile
        FindUsages -> findUsages compOpts srcpath ghcpath pos (moduleFile opts) files
        Test       -> print compOpts
