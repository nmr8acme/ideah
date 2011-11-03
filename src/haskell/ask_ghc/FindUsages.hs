module FindUsages (findUsages) where

import Control.Monad (when)
import System.Exit
import FastString (unpackFS)

import GHC
import MonadUtils
import Name hiding (varName)
import Id
import Var (varName)

import HUtil
import Walker
import GetDeclPos

findUsages :: String -> FilePath -> (Int, Int) -> FilePath -> IO ()
findUsages srcPath ghcPath (line, col) srcFile =
    runGhc (Just ghcPath) (doWalk srcPath srcFile (lineToGhc line) (colToGhc col))

extractDecl :: Int -> Int -> TypecheckedModule -> Id -> SrcSpan -> Where -> Ghc ()
extractDecl line col checked var loc _ =
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let cb = defWalkCallback { ident = extractLocs (varName var) }
        walkDeclarations cb (typecheckedSource checked)
        liftIO exitSuccess

extractLocs :: Name -> Id -> SrcSpan -> Where -> Ghc ()
extractLocs name var span _ = let loc     = srcSpanStart span
                                  varname = varName var
    in liftIO $ when (isGoodSrcSpan span && name == varname && loc /= nameSrcLoc varname) $ do
        print $ locStr loc
        print $ unpackFS $ srcLocFile loc

doExtractLocs :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractLocs line col checked = do
    let cb = defWalkCallback { ident = extractDecl line col checked }
    walkDeclarations cb (typecheckedSource checked)

doWalk :: String -> FilePath -> Int -> Int -> Ghc ()
doWalk srcPath srcFile line col = do
    setupFlags True ["-i" ++ srcPath]
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractLocs line col checked
