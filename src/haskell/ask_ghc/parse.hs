import Data.IORef

import GHC
import MonadUtils
import StringBuffer

import Walker
import HUtil

doPrintOut parsed = do
    indent <- liftIO $ newIORef ""
    let prt1 loc what = liftIO $ do
        i <- readIORef indent
        putStrLn (i ++ what ++ " " ++ spanStr loc ++ " {")
        writeIORef indent (i ++ "    ")
    let prt2 = liftIO $ do
        i <- readIORef indent
        let newi = take (length i - 4) i;
        writeIORef indent newi
        putStrLn (newi ++ "}")
    let f = defWalkCallback { braceOpen = prt1, braceClose = prt2 }
    mapM_ (walk f) (hsmodDecls parsed)

doWalk :: Ghc ()
doWalk = do
    setupFlags True ["-i."]
    buffer <- liftIO $ hGetStringBuffer "test.hs"
    result <- parseHsFile buffer "test.hs"
    case result of
        Right parsed -> doPrintOut (unLoc parsed)
        Left _ -> return ()

main = do
    runGhc (Just "C:\\Haskell\\lib") doWalk
    return ()
