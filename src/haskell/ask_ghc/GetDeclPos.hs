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

extractPos :: Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractPos line col var loc _ = liftIO $
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let loc' = nameSrcLoc $ idName var
        putStrLn $ locStr loc'
        print $ srcLocFile $ nameSrcLoc $ varName var
        exitSuccess

doExtractPos :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractPos line col checked = do
    let cb = defWalkCallback { ident = extractPos line col }
    walkDeclarations cb (typecheckedSource checked)

doWalk :: String -> FilePath -> Int -> Int -> Ghc ()
doWalk srcPath srcFile line col = do
    setupFlags True ["-i" ++ srcPath]
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractPos line col checked
