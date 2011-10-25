import GHC
import MonadUtils

import Walker
import HUtil
import HDebugUtil

doPrintOut parsed = do
    f <- printCallback defWalkCallback
    mapM_ (walk f) (hsmodDecls parsed)

doWalk :: Ghc ()
doWalk = do
    setupFlags True []
    buffer <- liftIO $ loadFile "test.hs"
    result <- parseHsFile buffer "test.hs"
    case result of
        Right parsed -> doPrintOut (unLoc parsed)
        Left _ -> return ()

main = do
    runGhc (Just "C:\\Haskell\\lib") doWalk
    return ()
