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
import FindUsages (extractModDecl)

getDeclPos :: [String] -> String -> FilePath -> (Int, Int) -> FilePath -> [FilePath] -> IO ()
getDeclPos compOpts srcPath ghcPath (line, col) srcFile files =
    runGhc (Just ghcPath) (doWalk compOpts srcPath srcFile (lineToGhc line) (colToGhc col) files)

extractPos outputLoc line col var loc _ = liftIO $
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        putStrLn $ locStr outputLoc
        putStrLn $ unpackFS $ srcLocFile outputLoc
        exitSuccess

extractIdPos :: Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractIdPos line col var loc _ = liftIO $
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let loc' = nameSrcLoc $ idName var
        putStrLn $ locStr loc'
        putStrLn $ unpackFS $ srcLocFile loc'
        exitSuccess

--extractModPos :: Int -> Int -> ModuleName -> SrcSpan -> WhereMod -> Ghc ()
--extractModPos line col mod span whereMod = e

doExtractIdPos :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractIdPos line col checked = do
    let cb = defWalkCallback { ident = extractIdPos line col }
    walkDeclarations cb (typecheckedSource checked)

doExtractModPos :: Int -> Int -> ParsedSource -> FilePath -> [FilePath] -> Ghc ()
doExtractModPos line col parsed srcFile files = do
    let cb = defWalkCallback { modName = extractModDecl True line col parsed srcFile files } -- extractModPos line col parsedSrc srcFile files }
    walkModule cb parsed

doWalk :: [String] -> String -> FilePath -> Int -> Int -> [FilePath] -> Ghc ()
doWalk compOpts srcPath srcFile line col files = do
    setupFlags True $ ("-i" ++ srcPath) : compOpts
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractIdPos line col checked
    doExtractModPos line col (parsedSource checked) srcFile files
