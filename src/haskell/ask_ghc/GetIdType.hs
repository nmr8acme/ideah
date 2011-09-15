module GetIdType (getIdType) where

import Control.Monad (when)
import System.Exit

import GHC
import MonadUtils
import Outputable
import Var
import TypeRep

import HUtil
import Walker

unForall :: Type -> Type
unForall (ForAllTy _ t) = unForall t
unForall t = t

-- todo: find nearest id
extractTypes :: PprStyle -> Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractTypes style line col var loc _ = liftIO $
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let ts = show $ pprType (unForall $ varType var) style
        putStrLn ts
        exitSuccess

doExtractTypes line col checked = do
    let info = tm_checked_module_info checked
    (Just unqual) <- mkPrintUnqualifiedForModule info
    let style = mkUserStyle unqual AllTheWay
    walkLBinds (defWalkCallback { generic = extractTypes style line col }) (typecheckedSource checked)

doWalk srcPath srcFile line col = do
    setupFlags True ["-i" ++ srcPath]
    addTargetFile srcFile
    load LoadAllTargets
    mods <- loadHsFile srcFile
    parsed <- parseModule $ head mods
    checked <- typecheckModule parsed
    doExtractTypes line col checked

getIdType srcPath ghcPath srcFile (line, col) = do
    runGhc (Just ghcPath) (doWalk srcPath srcFile (lineToGhc line) (colToGhc col))
    return ()
