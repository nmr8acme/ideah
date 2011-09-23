module HDebugUtil where

import Data.IORef

import MonadUtils

import HUtil
import Walker

printCallback cb = do
    indent <- liftIO $ newIORef ""
    let
        prt1 loc what = liftIO $ do
            i <- readIORef indent
            putStrLn (i ++ what ++ " " ++ spanStr loc ++ " {")
            writeIORef indent (i ++ "    ")
        prt2 = liftIO $ do
            i <- readIORef indent
            let newi = take (length i - 4) i;
            writeIORef indent newi
            putStrLn (newi ++ "}")
    return $ cb { braceOpen = prt1, braceClose = prt2 }
