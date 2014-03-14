module AllImports (allImportsInFile, importNamesInFiles) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import GHC
import MonadUtils

import HUtil

-- Run example:
-- ask_ghc.exe -m AllImports -g "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib" test.hs

-- Print out location spans of all imported modules in functions in the following format:
-- new-msg indicator
-- module name
-- loc span of module
-- imported function #1
-- loc span of function #1
-- imported function #2
-- loc span of function #2
-- ...
allImportsInFile compOpts ghcPath file = runGhc (Just ghcPath) getImportLocs
    where getImportLocs = do
            setupFlags True compOpts
            mod    <- loadHsFile file
            parsed <- parseModule mod
            printImports parsed True

-- Run example:
-- ask_ghc.exe -m AllImportsWithPkgs -g "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib" test.hs Testi.hs

-- Print out imported modules in the following format:
-- names of first package.module separated by new lines
-- new-msg indicator
-- names of 2nd package.module
-- names of 3nd package.module
-- ...
importNamesInFiles compOpts ghcPath files = runGhc (Just ghcPath) $ do
    setupFlags True compOpts
    printImportNames True $ head files
    mapM_ (printImportNames False) $ tail files
        where printImportNames isFirst file = do
                mod    <- loadHsFile file
                parsed <- parseModule mod
                printImports parsed False
                when isFirst $ liftIO $ putStrLn newMsgIndicator

-- when true, prints locations and newMsgIndicator and doesn't print package name (needed for optimize imports)
-- when false, doesn't print locations and newMsgIndicator, and prints package name (needed for creating import usage tree to prioritize imports for auto-importing)
type WithLocs = Bool

printImports :: ParsedModule -> WithLocs -> Ghc ()
printImports parsed withLocs = mapM_ (printImport withLocs) $ ms_textual_imps $ pm_mod_summary parsed

printImport :: WithLocs -> Located (ImportDecl RdrName) -> Ghc ()
printImport withLocs locImp =
    let imp = unLoc locImp
        name = ideclName imp
        loc  = getLoc locImp
        pkg  = fromMaybe "" $ fmap ((++ ".") . toString) $ ideclPkgQual imp -- todo pkg qualifier is never printed out
    in liftIO $
        if withLocs
            then do
                putStrLn newMsgIndicator
                putStrLn $ toString name
                putStrLn $ spanStr loc
                printFuncs $ ideclHiding imp
            else
                putStrLn $ pkg ++ toString name

printFuncs (Just (False, names)) = mapM_ printFunc names
printFuncs _                     = return ()

printFunc lie = do
    putStrLn $ toString $ unLoc lie
    putStrLn $ spanStr $ getLoc lie
