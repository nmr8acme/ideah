import Outputable
import GHC
import Var
import Name
import TypeRep

import HUtil
import Walker

unForall :: Type -> Type
unForall (ForAllTy _ t) = unForall t
unForall t = t

modName :: Name -> Maybe String
modName var = do
    md <- nameModule_maybe var
    return (toString $ moduleName md)

isDefinition :: Where -> Bool
isDefinition x = x == WTyDecl || x == WConDecl || x == WFunDecl || x == WFunDecl2 || x == WParam

extractId :: Name -> SrcSpan -> Where -> Ghc ()
extractId var loc w = do
    trace ((if isDefinition w then "!" else " ") ++ " " ++ toString loc ++ "\t\t" ++ (show $ modName var) ++ " \"" ++ (toString $ nameOccName var) ++ "\" @ " ++ (toString $ nameUnique var) ++ "\t" ++ show w) $ return ()

extractTypes :: PprStyle -> Id -> SrcSpan -> Where -> Ghc ()
extractTypes style var loc _ = do
    let ts = show $ pprType (unForall $ varType var) style
    trace (spanStr loc ++ " " ++ (toString $ Var.varName var) ++ ": " ++ ts) $ return ()
--extractTypes _ _ _ _ = return ()

doExtractTypes checked = do
    let info = tm_checked_module_info checked
    (Just unqual) <- mkPrintUnqualifiedForModule info
    let style = mkUserStyle unqual AllTheWay
    walkDeclarations (defWalkCallback { ident = extractTypes style }) (typecheckedSource checked)

doExtractIds checked = do
    let (Just (grp, _, _, _)) = renamedSource checked
    walkGroup (defWalkCallback { ident = extractId, name = extractId }) grp

doWalk :: Ghc ()
doWalk = do
    let file = "test.hs"
    setupFlags True ["-i."]
    mod <- loadHsFile file
    parsed <- parseModule mod
    checked <- typecheckModule parsed
    doExtractTypes checked
    --doExtractIds checked

main = do
    runGhc (Just "C:\\Haskell\\lib") doWalk
    return ()
