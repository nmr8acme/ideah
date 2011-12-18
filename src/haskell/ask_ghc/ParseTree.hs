module ParseTree (parseTree) where

import IO

import GHC
import MonadUtils

import Walker
import HUtil
import HDebugUtil

parseTree compOpts ghcPath file = runGhc (Just ghcPath) parseTree'
    where 
        parseTree' = do
            setupFlags True compOpts
            buffer <- liftIO loadStdin
            result <- parseHsFile buffer file
            case result of
                Right parsed -> doPrintOut parsed
                Left (_, err) -> liftIO $ hPutStrLn stderr err

doPrintOut parsed = do
    f <- printCallback defWalkCallback
    walkModule f parsed
