module FindUsages (findUsages) where

import Control.Monad (when)
import System.Exit
import FastString (unpackFS)

import GHC
import MonadUtils
import Name (nameSrcLoc)
import Id
import Var (varName)

import HUtil
import Walker
import GetDeclPos

findUsages :: String -> FilePath -> FilePath -> (Int, Int) -> IO ()
findUsages srcPath ghcPath srcFile (line, col) =
    runGhc (Just ghcPath) (doWalk srcPath srcFile (lineToGhc line) (colToGhc col))

extractLocs :: Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractLocs line col var span _ = let loc = srcSpanStart span
    in liftIO $ when (isGoodSrcSpan span && srcLocLine loc == line && srcLocCol loc == col) $ do
        putStrLn $ locStr loc
        print $ unpackFS $ srcLocFile $ nameSrcLoc $ varName var

doExtractLocs :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractLocs line col checked = do
    let cb = defWalkCallback { ident = extractLocs line col }
    walkDeclarations cb (typecheckedSource checked)

doWalk :: String -> FilePath -> Int -> Int -> Ghc ()
doWalk srcPath srcFile line col = do
    setupFlags True ["-i" ++ srcPath]
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractLocs line col checked
