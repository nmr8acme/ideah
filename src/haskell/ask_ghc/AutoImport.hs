module AutoImport (autoImport) where

import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import GHC
import MonadUtils

import HUtil

-- ask_ghc.exe -m AutoImport -n intersperse -g "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib" test.hs

inspect name module' = do
    (Just mi)    <- getModuleInfo module'
    let exports   = modInfoExports mi
    let filtered  = getModule name `mapMaybe` exports
    mapM_ printModExports filtered

-- 'files' consists of all user files that should be looked for identifier.
-- It should not contain the source file.
autoImport compOpts name ghcPath srcPath files = runGhc (Just ghcPath) $ do
    setupFlags True $ ("-i" ++ srcPath) : compOpts
    pkgModules  <- packageDbModules True
    userModSums <- mapM loadHsFile files
    let userMods = ms_mod `map` userModSums
    mapM_ (inspect name) $ userMods ++ pkgModules

getModule functionName module' =
    let fullNameStr = toString module'
    in  if functionName == fullNameStr || ('.' : functionName) `isSuffixOf` fullNameStr
        then Just fullNameStr
        else Nothing

printModExports = liftIO . putStrLn