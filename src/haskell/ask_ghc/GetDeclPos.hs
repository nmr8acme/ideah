module GetDeclPos (getDeclPos) where

import Control.Monad (when)
import System.Exit

import GHC
import MonadUtils
import Name (nameSrcLoc)
import Id
import Var (varName)

import HUtil
import Walker

getDeclPos :: String -> FilePath -> FilePath -> (Int, Int) -> IO ()
getDeclPos srcPath ghcPath srcFile (line, col) =
    runGhc (Just ghcPath) (doWalk srcPath srcFile (lineToGhc line) (colToGhc col))

extractLoc :: Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractLoc line col var loc _ = liftIO $
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let loc = nameSrcLoc $ idName var
        print $ srcLocLine loc
        print $ colFromGhc $ srcLocCol loc
        print $ srcLocFile $ nameSrcLoc $ varName var
        exitSuccess

doExtractLoc :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractLoc line col checked = do
    let cb = defWalkCallback { ident = extractLoc line col }
    walkDeclarations cb (typecheckedSource checked)

doWalk :: String -> FilePath -> Int -> Int -> Ghc ()
doWalk srcPath srcFile line col = do
    setupFlags True ["-i" ++ srcPath]
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractLoc line col checked
