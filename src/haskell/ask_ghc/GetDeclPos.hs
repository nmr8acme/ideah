module GetDeclPos (getDeclPos) where

import Control.Monad (when)
import System.Exit

import GHC
import MonadUtils
import Name (nameSrcLoc)
import FastString (unpackFS)
import Id
import HUtil
import Walker

getDeclPos :: [String] -> String -> FilePath -> FilePath -> (Int, Int) -> IO ()
getDeclPos compOpts srcPath ghcPath srcFile (line, col) =
    runGhc (Just ghcPath) (doWalk compOpts srcPath srcFile (lineToGhc line) (colToGhc col))

extractPos :: Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractPos line col var loc _ = liftIO $
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let loc' = nameSrcLoc $ idName var
        putStrLn $ locStr loc'
        putStrLn $ unpackFS $ srcLocFile loc'
        exitSuccess

doExtractPos :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractPos line col checked = do
    let cb = defWalkCallback { ident = extractPos line col }
    walkDeclarations cb (typecheckedSource checked)

doWalk :: [String] -> String -> FilePath -> Int -> Int -> Ghc ()
doWalk compOpts srcPath srcFile line col = do
    setupFlags True $ ("-i" ++ srcPath) : compOpts
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractPos line col checked
