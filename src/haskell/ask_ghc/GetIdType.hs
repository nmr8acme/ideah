module GetIdType (getIdType) where

import Control.Monad (when)
import System.Exit

import GHC
import MonadUtils
import Outputable
import Name (nameModule_maybe)
import Var
import TypeRep

import HUtil
import Walker

unForall :: Type -> Type
unForall (ForAllTy _ t) = unForall t
unForall t = t

extractTypes :: PprStyle -> Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractTypes style line col var loc _ = liftIO $
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let ts = show $ pprType (unForall $ varType var) style
        let ns = maybe "" (toString . moduleName) (nameModule_maybe $ varName var)
        putStrLn ns
        putStrLn newMsgIndicator
        putStrLn ts
        exitSuccess

doExtractTypes :: Int -> Int -> TypecheckedModule -> Ghc ()
doExtractTypes line col checked = do
    let info = tm_checked_module_info checked
    (Just unqual) <- mkPrintUnqualifiedForModule info
    let style = mkUserStyle unqual AllTheWay
    let cb = defWalkCallback { generic = extractTypes style line col }
    --cb <- printCallback defWalkCallback
    walkDeclarations cb (typecheckedSource checked)

doWalk :: String -> FilePath -> Int -> Int -> Ghc ()
doWalk srcPath srcFile line col = do
    setupFlags True ["-i" ++ srcPath]
    mod <- loadHsFile srcFile
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractTypes line col checked

getIdType :: String -> FilePath -> FilePath -> (Int, Int) -> IO ()
getIdType srcPath ghcPath srcFile (line, col) =
    runGhc (Just ghcPath) (doWalk srcPath srcFile (lineToGhc line) (colToGhc col))
