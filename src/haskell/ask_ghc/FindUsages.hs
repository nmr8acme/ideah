module FindUsages (findUsages, extractModDecl) where

import Control.Monad (when)
import System.Exit
import System.FilePath

import GHC
import MonadUtils
import Name hiding (varName)
import FastString (unpackFS)
import Var (varName)

import HUtil
import Walker

findUsages :: [String] -> String -> FilePath -> (Int, Int) -> FilePath -> [FilePath] -> IO ()
findUsages compOpts srcPath ghcPath (line, col) srcFile files =
    runGhc (Just ghcPath) (doWalk compOpts srcPath srcFile (lineToGhc line) (colToGhc col) files)

extractDecl walkFun otherModulesWalkFun line col src srcFile files _ loc _ =
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        mapM_ (\file -> if equalFilePath srcFile file
                then walkFun src
                else do
                    modF     <- loadHsFile file
                    parsedF  <- parseModule modF
                    checkedF <- typecheckModule parsedF
                    walkFun $ otherModulesWalkFun checkedF) files
        liftIO exitSuccess

extractIdDecl :: Int -> Int -> TypecheckedModule -> FilePath -> [FilePath] -> Id -> SrcSpan -> Where -> Ghc ()
extractIdDecl line col src srcFile files var = extractDecl
        (walkDeclarations (defWalkCallback { ident = extractIdLocs (varName var) }) . typecheckedSource)
        id line col src srcFile files var

extractModDecl :: Bool -> Int -> Int -> ParsedSource -> FilePath -> [FilePath] -> ModuleName -> SrcSpan -> WhereMod -> Ghc ()
extractModDecl onlyDecl line col src srcFile files mod = extractDecl
    (walkModule (defWalkCallback { modName = extractModLocs onlyDecl mod }))
    parsedSource line col src srcFile files mod

extractLocs rightName span _ = let loc = srcSpanStart span
    in liftIO $ when (isGoodSrcSpan span && rightName) $ liftIO $ do
        putStrLn $ locStr loc
        putStrLn $ unpackFS $ srcLocFile loc

extractModLocs :: Bool -> ModuleName -> ModuleName -> SrcSpan -> WhereMod -> Ghc ()
extractModLocs onlyDecl name mod span whereMod = extractLocs
    (name == mod && (not onlyDecl || whereMod == WMModule)) span whereMod  -- && loc /= nameSrcLoc varname)

extractIdLocs :: Name -> Id -> SrcSpan -> Where -> Ghc ()
extractIdLocs name var span = let varname = varName var
    in extractLocs (name == varname && srcSpanStart span /= nameSrcLoc varname) span

doExtractIdLocs :: Int -> Int -> TypecheckedModule -> FilePath -> [FilePath] -> Ghc ()
doExtractIdLocs line col checkedSrc srcFile files = do
    let cb = defWalkCallback { ident = extractIdDecl line col checkedSrc srcFile files }
    walkDeclarations cb (typecheckedSource checkedSrc)

doExtractModLocs :: Int -> Int -> ParsedSource -> FilePath -> [FilePath] -> Ghc ()
doExtractModLocs line col parsedSrc srcFile files = do
    let cb = defWalkCallback { modName = extractModDecl False line col parsedSrc srcFile files }
    walkModule cb parsedSrc

doWalk :: [String] -> String -> FilePath -> Int -> Int -> [FilePath] -> Ghc ()
doWalk compOpts srcPath srcFile line col files = do
    setupFlags True $ ("-i" ++ srcPath) : compOpts
    modSrc     <- loadHsFile srcFile
    parsedSrc  <- parseModule modSrc
    checkedSrc <- typecheckModule parsedSrc
    doExtractIdLocs line col checkedSrc srcFile files
    doExtractModLocs line col (parsedSource checkedSrc) srcFile files
