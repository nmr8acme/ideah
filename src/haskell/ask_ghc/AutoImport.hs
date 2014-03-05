module AutoImport (allImports, autoImport) where

import GHC
import MonadUtils

import HUtil

-- ask_ghc.exe -m AutoImport -n intersperse -g "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib" test.hs

inspect module' = do
    (Just mi)    <- getModuleInfo module'
    let exports   = map toString $ modInfoExports mi
    mapM_ (liftIO . putStrLn) exports

allImports ghcPath = runGhc (Just ghcPath) $ do
    setupFlags True []
    pkgModules <- packageDbModules True
    mapM_ inspect pkgModules

-- 'files' consists of all user module files.
autoImport compOpts ghcPath srcPath files = runGhc (Just ghcPath) $ do
    let modFiles = filter isModuleFile files
    setupFlags True $ ("-i" ++ srcPath) : compOpts
    userModSums <- mapM loadHsFile modFiles
    let userMods = ms_mod `map` userModSums
    mapM_ inspect userMods
