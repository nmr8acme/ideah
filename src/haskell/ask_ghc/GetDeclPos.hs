module GetDeclPos (getDeclPos) where

import Control.Monad (when)
import System.Exit

import GHC
import MonadUtils
import Name (nameSrcLoc)
import Id
import HUtil
import Walker
import FindUsages (extractModDecl)

getDeclPos :: [String] -> String -> FilePath -> (Int, Int) -> FilePath -> [FilePath] -> IO ()
getDeclPos compOpts srcPath ghcPath (line, col) srcFile files =
    runGhc (Just ghcPath) (doWalk compOpts srcPath srcFile (lineToGhc line) (colToGhc col) files)

extractPos outputLoc line col var loc _ = liftIO $
    when (isLoc loc line col) $ do
        putStrLn $ locStr outputLoc
        putStrLn $ locFileName outputLoc
        exitSuccess

extractIdPos :: Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractIdPos line col var loc _ = liftIO $
    when (isLoc loc line col) $ do
        let loc' = nameSrcLoc $ idName var
        putStrLn $ locStr loc'
        putStrLn $ locFileName loc'
        exitSuccess

doExtractIdPos :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractIdPos line col checked = do
    let cb = defWalkCallback { ident = extractIdPos line col }
    walkDeclarations cb (typecheckedSource checked)

doExtractModPos :: Int -> Int -> ParsedSource -> FilePath -> [FilePath] -> Ghc ()
doExtractModPos line col parsed srcFile files = do
    let cb = defWalkCallback { modName = extractModDecl True line col parsed srcFile files }
    walkModule cb parsed

doWalk :: [String] -> String -> FilePath -> Int -> Int -> [FilePath] -> Ghc ()
doWalk compOpts srcPath srcFile line col files = do
    setupFlags True $ ("-i" ++ srcPath) : compOpts
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractIdPos line col checked
    doExtractModPos line col (parsedSource checked) srcFile files
