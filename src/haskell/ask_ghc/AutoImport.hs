module AutoImport (allImports, autoImport) where

import Control.Monad (when)
import Data.List (isInfixOf)

import GHC
import MonadUtils

import HUtil

inspect module' = do
    (Just mi)  <- getModuleInfo module'
    let modName = toString (moduleName module') ++ "."
    let exports = map (\e -> modName ++ toStringSimple e) $ modInfoExports mi
    -- temporary solution to
    -- "Control.Category.Unicode.ask_ghc.exe: <stdout>: commitBuffer: invalid argument (invalid character)
    let unicodePackages = ["Control.Category.Unicode", "Control.Applicative.Unicode", "Control.Arrow.Unicode", "Control.Monad.Unicode", "Data.Bool.Unicode", "Data.Eq.Unicode", "Data.Foldable.Unicode", "Data.Function.Unicode", "Data.List.Unicode", "Data.Monoid.Unicode", "Data.Ord.Unicode", "Prelude.Unicode"]
    let doWrite s = not $ any (flip isInfixOf s) unicodePackages
    mapM_ (\e -> liftIO $ when (doWrite e) $ putStrLn e) exports

-- Example:
-- ask_ghc.exe -m AutoImport -g "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib"

allImports ghcPath = runGhc (Just ghcPath) $ do
    setupFlags True []
    pkgModules <- packageDbModules True
    mapM_ inspect pkgModules

-- Example:
-- ask_ghc.exe -m AutoImport -g "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib" test.hs test2.hs

-- 'files' consists of all user module files.
autoImport compOpts ghcPath srcPath files = runGhc (Just ghcPath) $ do
    let modFiles = filter isModuleFile files
    setupFlags True $ ("-i" ++ srcPath) : compOpts
    userModSums <- mapM loadHsFile modFiles
    let userMods = ms_mod `map` userModSums
    mapM_ inspect userMods
