module ImportEnd (getImportEnd) where

import Data.List (find)
import Data.Maybe (catMaybes)

import GHC
import MonadUtils
import SrcLoc (advanceSrcLoc)

import HUtil

-- Run example:
-- ask_ghc.exe -m ImportEnd -g "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib" -n "Data.List" test.hs

-- Find the position where a given module should be imported
getImportEnd compOpts ghcPath moduleName file = runGhc (Just ghcPath) getImportEnd'
    where getImportEnd' = do
            setupFlags True compOpts
            mod    <- loadHsFile file
            parsed <- parseModule mod
            findEnd moduleName parsed

findEnd :: String -> ParsedModule -> Ghc ()
findEnd moduleName parsed =
    let sourceImports = ms_textual_imps $ pm_mod_summary parsed -- todo test with no imports (does implicit Prelude import have location?)
        imports       = unLoc `map` sourceImports
    in printImport $ case existingImport imports moduleName of
          Just existing -> existing
          Nothing       -> nextImportLine imports

-- If the module isn't imported, return the line followring the last import
nextImportLine :: [ImportDecl RdrName] -> RealSrcLoc
nextImportLine imports =
    let spans      = map (getLoc . ideclName) imports
        realLocs   = catMaybes . map realLoc
        lastImport = maximum $ realLocs $ map srcSpanEnd spans
    in  advanceSrcLoc lastImport '\n'

realLoc :: SrcLoc -> Maybe RealSrcLoc
realLoc (RealSrcLoc r) = Just r
realLoc _              = Nothing

-- If the module is already imported, find the position of the last imported function
existingImport :: [ImportDecl RdrName] -> String -> Maybe RealSrcLoc
existingImport imports moduleName = do
    existing        <- find (\imp -> toString (ideclName imp) == moduleName) imports
    (isHiding, ies) <- ideclHiding existing
    if (not isHiding)
        then realLoc $ maximum $ map (srcSpanEnd . getLoc) ies
        else error "Trying to import module that is already imported" -- todo

printImport :: RealSrcLoc -> Ghc ()
printImport = liftIO . putStrLn . realLocStr
