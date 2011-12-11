module FindUsages (findUsages) where

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

findUsages :: String -> FilePath -> (Int, Int) -> FilePath -> [FilePath] -> IO ()
findUsages srcPath ghcPath (line, col) srcFile files =
    runGhc (Just ghcPath) (doWalk srcPath srcFile (lineToGhc line) (colToGhc col) files)

extractDecl :: Int -> Int -> TypecheckedModule -> FilePath -> [FilePath] -> Id -> SrcSpan -> Where -> Ghc ()
extractDecl line col checkedSrc srcFile files var loc _ =
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let cb = defWalkCallback { ident = extractLocs (varName var) }
        mapM_ (\file -> let walkDecls = walkDeclarations cb . typecheckedSource
            in if equalFilePath srcFile file
                then walkDecls checkedSrc
                else do
                    modF     <- loadHsFile file
                    parsedF  <- parseModule modF
                    checkedF <- typecheckModule parsedF
                    walkDecls checkedF) files
        liftIO exitSuccess

extractLocs :: Name -> Id -> SrcSpan -> Where -> Ghc ()
extractLocs name var span _ = let loc     = srcSpanStart span
                                  varname = varName var
    in liftIO $ when (isGoodSrcSpan span && name == varname && loc /= nameSrcLoc varname) $ do
        putStrLn $ locStr loc
        putStrLn $ unpackFS $ srcLocFile loc

doExtractLocs :: Int -> Int -> TypecheckedModule -> FilePath -> [FilePath] -> Ghc ()
doExtractLocs line col checkedSrc srcFile files = do
    let cb = defWalkCallback { ident = extractDecl line col checkedSrc srcFile files }
    walkDeclarations cb (typecheckedSource checkedSrc)

doWalk :: String -> FilePath -> Int -> Int -> [FilePath] -> Ghc ()
doWalk srcPath srcFile line col files = do
    setupFlags True ["-i" ++ srcPath]
    modSrc     <- loadHsFile srcFile
    parsedSrc  <- parseModule modSrc
    checkedSrc <- typecheckModule parsedSrc
    doExtractLocs line col checkedSrc srcFile files
