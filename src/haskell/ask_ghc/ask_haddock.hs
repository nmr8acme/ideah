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
    [ moduleOption
    , ghcpathOption
    , sourcepathOption
    , lineNumberOption
    , columnNumberOption
    , ghcoptionsOption
    ]

main = do
    args <- getArgs
    let (opts', files, errors) = getOpt Permute options args
    unless (null errors) $ ioError $ userError $ concat errors
    let opts = foldl (\opt f -> f opt) defaultOpts opts'
    getDocu (compilerOptions opts) (sourcePath opts) (ghcPath opts) (position opts) $ moduleFile opts
