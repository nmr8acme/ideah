module Main where

import Control.Monad
import System.Environment
import System.Console.GetOpt

import HUtil
import AllImports
import AutoImport
import Compile
import CheckMain
import GetIdType
import GetDeclPos
import ImportEnd
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
--    -n <identifier>     # name of unresolved identifier to be looked up in other modules
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
          , nameOption
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
        ident      = identifier opts
    case mode opts of
        AllImports         -> allImportsInFile compOpts ghcpath singleFile
        AllImportsWithPkgs -> importNamesInFiles compOpts ghcpath files
        AutoImport         -> if null files then allImports ghcpath
                                            else autoImport compOpts ghcpath srcpath files
        Compile            -> compile (outputPath opts) srcpath ghcpath compOpts files
        CheckMain          -> checkMain compOpts ghcpath singleFile
        FindUsages         -> findUsages compOpts srcpath ghcpath pos (moduleFile opts) files
        GetDeclPos         -> getDeclPos compOpts srcpath ghcpath pos (moduleFile opts) files
        GetIdType          -> getIdType compOpts srcpath ghcpath singleFile pos
        Help               -> putStrLn $ usageInfo "Usage: ask_ghc [OPTION...] files...\n" options
        ImportEnd          -> getImportEnd compOpts ghcpath ident singleFile
        ParseTree          -> parseTree compOpts ghcpath singleFile
        Test               -> print compOpts
