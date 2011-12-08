module Main where

import Control.Monad
import System.Environment
import System.Console.GetOpt

import HUtil
import GetDocu

-- ./ask_haddock
--    -f                  # module for specified line and column numbers
--    -g <path>           # ghc lib path
--    -s <path>           # source path
--    -l <line>           # line number
--    -r <column>         # column number
--    <files>             # files to be compiled

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['f'] ["module"]         (ReqArg (\modf opt -> opt {moduleFile = modf}) "String") "Module for specified line and column numers"
    , Option ['g'] ["ghcpath"]        (ReqArg (\path opt -> opt {ghcPath = path}) "DIR") "GHC path"
    , Option ['s'] ["sourcepath"]     (ReqArg (\path opt -> opt {sourcePath = path}) "DIR") "source path"
    , Option ['l'] ["line-number"]    (ReqArg (\line opt -> opt {position = (read line, snd $ position opt)}) "Num") "line number"
    , Option ['r'] ["column-number"]  (ReqArg (\col opt  -> opt {position = (fst $ position opt, read col)}) "Num") "column number"
    ]

main = do
    args <- getArgs
    let (opts', files, errors) = getOpt Permute options args
    unless (null errors) $ ioError $ userError $ concat errors
    let opts = foldl (\opt f -> f opt) defaultOpts opts'
    getDocu (sourcePath opts) (ghcPath opts) (position opts) $ moduleFile opts
