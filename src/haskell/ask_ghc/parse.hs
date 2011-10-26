import GHC
import MonadUtils

import Walker
import HUtil
import HDebugUtil

doPrintOut parsed = do
    f <- printCallback defWalkCallback
    mapM_ (walk f) (hsmodDecls parsed)

doWalk :: String -> Ghc ()
doWalk file = do
    setupFlags True []
    buffer <- liftIO $ loadFile file
    result <- parseHsFile buffer file
    case result of
        Right parsed -> doPrintOut (unLoc parsed)
        Left _ -> return ()

main = do
    runGhc (Just "C:\\Haskell\\lib") $ doWalk "test.hs"
    return ()
