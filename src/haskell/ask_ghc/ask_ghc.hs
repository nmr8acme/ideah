module Main where

import Control.Monad
import System.Environment
import System.Console.GetOpt

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
options = [ mainFuncModeOption
        , moduleOption
        , ghcpathOption
        , outpathOption
        , sourcepathOption
        , ghcoptionsOption
        , lineNumberOption
        , columnNumberOption
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
        GetDeclPos -> getDeclPos compOpts srcpath ghcpath pos (moduleFile opts) files
        ParseTree  -> parseTree compOpts ghcpath singleFile
        FindUsages -> findUsages compOpts srcpath ghcpath pos (moduleFile opts) files
        Test       -> print compOpts
