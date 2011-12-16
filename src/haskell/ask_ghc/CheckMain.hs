module CheckMain (checkMain) where

import GHC
import MonadUtils

import HUtil

checkMain compOpts ghcPath file = runGhc (Just ghcPath) checkMain'
    where 
        checkMain' = do
            setupFlags True compOpts
            buffer <- liftIO loadStdin
            result <- parseHsFile buffer file
            liftIO $ putStrLn $ if hasMain result then "t" else "f"

hasMain result = case result of
    Right parsed -> any isMain $ map unLoc (hsmodDecls $ unLoc parsed)
    Left _       -> False

isMain (ValD (FunBind funid _ _ _ _ _)) = toString (unLoc funid) == "main"
isMain _                                = False
