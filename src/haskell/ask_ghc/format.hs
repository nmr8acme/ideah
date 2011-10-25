import System.Environment

import MonadUtils
import GHC

import HUtil

prLoc :: (MonadIO m) => String -> Located a -> m ()
prLoc what loc = liftIO $ putStrLn $ what ++ ": " ++ (spanStr $ getLoc loc)

doWalk :: [String] -> Ghc ()
doWalk src = do
    setupFlags True ["-i."]
    addTarget Target { targetId = TargetFile "" Nothing, targetAllowObjCode = False, targetContents = Just src }
    g <- depanal [] True
    parsed <- parseModule $ head g
    let md = unLoc $ pm_parsed_source parsed
{-
    case hsmodName md of
        Just mn -> liftIO $ putStrLn $ spanStr $ getLoc mn
        _ -> return ()
-}
    mapM_ (prLoc "import") (hsmodImports md)
    mapM_ (prLoc "decl") (hsmodDecls md)
    return ()

main = do
    args <- getArgs
    runGhc (Just "C:\\Haskell\\lib") (doWalk args)
    return ()
