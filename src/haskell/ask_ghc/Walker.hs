module Walker (
    Where(..), WhereMod(..), Callback(..), defWalkCallback,
    walk, walkDeclarations, walkGroup, walkModule, walkRenamed
    ) where

import Control.Monad (when)
import Data.Maybe (listToMaybe)

import Bag
import GHC
import BasicTypes
import DataCon

data Where = WTyDecl | WConDecl | WFunDecl | WFunDecl2 | WParam | WVal | WCon | WType | WMatch | WModule | WSig
    deriving (Show, Eq)

data WhereMod = WMModule | WMImport
    deriving (Show, Eq)

data Callback a m = CB { ident      :: a -> SrcSpan -> Where -> m (),
                         name       :: Name -> SrcSpan -> Where -> m (),
                         modName    :: ModuleName -> SrcSpan -> WhereMod -> m (),
                         braceOpen  :: SrcSpan -> String -> m (),
                         braceClose :: m () }

defWalkCallback :: (Monad m) => Callback a m
defWalkCallback = CB { ident      = \_ _ _ -> return (),
                       name       = \_ _ _ -> return (),
                       modName    = \_ _ _ -> return (),
                       braceOpen  = \_ _ -> return (),
                       braceClose = return () }

brace :: (Monad m) => Callback a m -> SrcSpan -> String -> m() -> m ()
brace f loc what inside = do
    (braceOpen f) loc what
    inside
    braceClose f

walkLoc :: b -> (b -> SrcSpan -> a -> m ()) -> Located a -> m ()
walkLoc cb walker node = walker cb (getLoc node) (unLoc node)


walkId' :: (Monad m) => Callback a m -> a -> SrcSpan -> Where -> m ()
walkId' f name loc definition = brace f loc "Id" $ (ident f) name loc definition

walkId :: (Monad m) => Callback a m -> Located a -> Where -> m ()
walkId f name definition = walkId' f (unLoc name) (getLoc name) definition


walkBinds :: (Monad m) => Callback a m -> Maybe a -> HsValBindsLR a a -> m ()
walkBinds f outer (ValBindsIn binds sigs) = do
    walkLBinds f outer binds
    mapM_ (walkLSig f) sigs
walkBinds f outer (ValBindsOut binds sigs) = do
    mapM_ (walkLBinds f outer) bags
    mapM_ (walkLSig defWalkCallback { ident = \n loc w -> (name f) n loc w }) sigs
    where bags = map snd binds -- list of bags

walkLocals :: (Monad m) => Callback a m -> HsLocalBinds a -> m ()
walkLocals f (HsValBinds binds) = walkBinds f Nothing binds
walkLocals f (HsIPBinds (IPBinds binds _)) = mapM_ walkIP binds
    where walkIP lip = do
              walkId' f id (getLoc lip) WParam
              walkLExpr f expr
              where (IPBind (IPName id) expr) = unLoc lip
walkLocals _f EmptyLocalBinds = return ()

walkRHSs :: (Monad m) => Callback a m -> GRHSs a -> m ()
walkRHSs f (GRHSs grhs locals) = do
        mapM_ (walkLoc f walkRHS) grhs
        walkLocals f locals
    where walkRHS f loc (GRHS stmts expr) = brace f loc "GRHS" $ do
              mapM_ (walkLStmt f) stmts
              walkLExpr f expr

walkMatchGroup :: (Monad m) => Callback a m -> Maybe a -> MatchGroup a -> m ()
walkMatchGroup f outer (MatchGroup matches _) = mapM_ (walkLoc f walkMatch) matches
    where walkMatch f loc (Match pats _ rhss) = brace f loc "Match" $ do
              maybe (return ()) (\outerFunc -> walkId' f outerFunc loc WMatch) outer
              mapM_ (walkLPattern f) pats
              walkRHSs f rhss

----------------------------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------------------------

walkLType :: (Monad m) => Callback a m -> LHsType a -> m ()
walkLType f typ = walkType f (getLoc typ) (unLoc typ)

walkType :: (Monad m) => Callback a m -> SrcSpan -> HsType a -> m ()
walkType f loc (HsForAllTy fa _ _ctx typ) = case fa of
        Explicit -> brace f loc "HsForAllTy" $ walkDown
        Implicit -> walkDown
    where walkDown = walkLType f typ
walkType f loc (HsTyVar name) = brace f loc "HsTyVar" $ walkId' f name loc WType
walkType f loc (HsAppTy typ1 typ2) = brace f loc "HsAppTy" $ do
    walkLType f typ1
    walkLType f typ2
walkType f loc (HsFunTy typ1 typ2) = brace f loc "HsFunTy" $ do
    walkLType f typ1
    walkLType f typ2
walkType f loc (HsListTy typ) = brace f loc "HsListTy" $ walkLType f typ
walkType f loc (HsPArrTy typ) = brace f loc "HsPArrTy" $ walkLType f typ
walkType f loc (HsTupleTy _ types) = brace f loc "HsTupleTy" $ mapM_ (walkLType f) types
walkType f loc (HsOpTy typ1 name typ2) = brace f loc "HsOpTy" $ do
    walkLType f typ1
    walkId f name WCon
    walkLType f typ2
walkType f loc (HsParTy typ) = brace f loc "HsParTy" $ walkLType f typ
walkType f loc (HsNumTy _) = brace f loc "HsNumTy" $ return ()
walkType f loc (HsPredTy _) = brace f loc "HsPredTy" $ return ()
walkType f loc (HsKindSig typ _) = brace f loc "HsKindSig" $ walkLType f typ
walkType f loc (HsDocTy typ _) = brace f loc "HsDocTy" $ walkLType f typ
#if __GLASGOW_HASKELL__ >= 700
walkType f loc (HsSpliceTy _ _ _) = brace f loc "HsSpliceTy" $ return ()
#else
walkType f loc (HsSpliceTy _) = brace f loc "HsSpliceTy" $ return ()
walkType f loc (HsSpliceTyOut _) = brace f loc "HsSpliceTyOut" $ return ()
#endif
walkType f loc (HsBangTy _ typ) = brace f loc "HsBangTy" $ walkLType f typ
walkType f loc (HsRecTy _) = brace f loc "HsRecTy" $ return () -- todo
#if __GLASGOW_HASKELL__ >= 700
walkType f loc (HsQuasiQuoteTy _) = brace f loc "HsQuasiQuoteTy" $ return ()
walkType f loc (HsCoreTy _) = brace f loc "HsCoreTy" $ return ()
#endif

----------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------

walkLStmt :: (Monad m) => Callback a m -> LStmt a -> m ()
walkLStmt f stmt = walkStmt f (getLoc stmt) (unLoc stmt)

walkStmt :: (Monad m) => Callback a m -> SrcSpan -> Stmt a -> m ()
-- pat <- expr statement in do
walkStmt f loc (BindStmt pat expr _ _) = brace f loc "BindStmt" $ do
    walkLPattern f pat
    walkLExpr f expr
-- expr in do
walkStmt f loc (ExprStmt expr _ _) = brace f loc "ExprStmt" $ walkLExpr f expr
-- let in do
walkStmt f loc (LetStmt binds) = brace f loc "LetStmt" $ walkLocals f binds
-- ???
walkStmt f loc (ParStmt _) = brace f loc "ParStmt" $ return ()
#if __GLASGOW_HASKELL__ >= 700
walkStmt f loc (TransformStmt _ _ _ _) = brace f loc "TransformStmt" $ return ()
walkStmt f loc (GroupStmt _ _ _ _) = brace f loc "GroupStmt" $ return ()
#else
walkStmt f loc (TransformStmt _ _ _) = brace f loc "TransformStmt" $ return ()
walkStmt f loc (GroupStmt _ _) = brace f loc "GroupStmt" $ return ()
#endif
walkStmt f loc (RecStmt _ _ _ _ _ _ _ _) = brace f loc "RecStmt" $ return ()

----------------------------------------------------------------------------------------------
-- Patterns
----------------------------------------------------------------------------------------------

walkConPat :: (Monad m) => Callback a m -> HsConPatDetails a -> m ()
walkConPat f details = mapM_ (walkLPattern f) (hsConPatArgs details)

walkLPattern :: (Monad m) => Callback a m -> LPat a -> m ()
walkLPattern f lpat = walkPattern f (getLoc lpat) (unLoc lpat)

walkPattern :: (Monad m) => Callback a m -> SrcSpan -> Pat a -> m ()
-- wildcard pattern (_)
walkPattern f loc (WildPat _) = brace f loc "WildPat" $ return ()
-- variable pattern (matches any value)
walkPattern f loc (VarPat id) = brace f loc "VarPat" $ walkId' f id loc WParam
-- ???
walkPattern f loc (VarPatOut id _) = brace f loc "VarPatOut" $ walkId' f id loc WParam
-- lazy pattern
walkPattern f loc (LazyPat pat) = brace f loc "LazyPat" $ walkLPattern f pat
-- as pattern (@)
walkPattern f loc (AsPat id pat) = brace f loc "AsPat" $ do
    walkId f id WParam
    walkLPattern f pat
-- parenthesis pattern (pat)
walkPattern f loc (ParPat pat) = brace f loc "ParPat" $ walkLPattern f pat
-- bang pattern
walkPattern f loc (BangPat pat) = brace f loc "BangPat" $ walkLPattern f pat
-- list pattern [a,b,c]
walkPattern f loc (ListPat pats _) = brace f loc "ListPat" $ mapM_ (walkLPattern f) pats
-- tuple pattern (x, y, z)
walkPattern f loc (TuplePat pats _ _) = brace f loc "TuplePat" $ mapM_ (walkLPattern f) pats
-- parallel array pattern 
walkPattern f loc (PArrPat pats _) = brace f loc "PArrPat" $ mapM_ (walkLPattern f) pats
-- constructor pattern
walkPattern f loc (ConPatIn id details) = brace f loc "ConPatIn" $ do
    walkId f id WCon
    walkConPat f details
-- constructor pattern after typecheck
walkPattern f loc (ConPatOut id _ _ _ details _) = brace f loc "ConPatOut" $ do
    (name f) (dataConName $ unLoc id) (getLoc id) WCon
    walkConPat f details
-- view pattern
walkPattern f loc (ViewPat expr pat _) = brace f loc "ViewPat" $ do
    walkLExpr f expr
    walkLPattern f pat
walkPattern f loc (QuasiQuotePat _) = brace f loc "QuasiQuotePat" $ return ()
-- literal pattern ("string")
walkPattern f loc (LitPat _) = brace f loc "LitPat" $ return ()
-- numeric (or any overloaded literal) pattern
walkPattern f loc (NPat _ _ _) = brace f loc "NPat" $ return ()
-- n+k pattern
walkPattern f loc (NPlusKPat _ _ _ _) = brace f loc "NPlusKPat" $ return ()
walkPattern f loc (TypePat typ) = brace f loc "TypePat" $ walkLType f typ
walkPattern f loc (SigPatIn _ _) = brace f loc "SigPatIn" $ return ()
walkPattern f loc (SigPatOut _ _) = brace f loc "SigPatOut" $ return ()
walkPattern f loc (CoPat _ _ _) = brace f loc "CoPat" $ return ()

----------------------------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------------------------

walkLExpr :: (Monad m) => Callback a m -> LHsExpr a -> m ()
walkLExpr f lexpr = walkExpr f (getLoc lexpr) (unLoc lexpr)

walkExpr :: (Monad m) => Callback a m -> SrcSpan -> HsExpr a -> m ()
-- named reference
walkExpr f loc (HsVar var) = brace f loc "HsVar" $ walkId' f var loc WVal
-- implicit parameter
walkExpr f loc (HsIPVar (IPName id)) = brace f loc "HsIPVar" $ walkId' f id loc WVal
-- overloaded literal (number)
walkExpr f loc (HsOverLit _) = brace f loc "HsOverLit" $ return ()
-- simple literal ("string", etc)
walkExpr f loc (HsLit _) = brace f loc "HsLit" $ return ()
-- lambda expression (\pattern -> body)
walkExpr f loc (HsLam mg) = brace f loc "HsLam" $ walkMatchGroup f Nothing mg
-- function application (f x)
walkExpr f loc (HsApp func param) = brace f loc "HsApp" $ do
    walkLExpr f func
    walkLExpr f param
-- infix operation application (a + b)
walkExpr f loc (OpApp left op _ right) = brace f loc "OpApp" $ do
    walkLExpr f left
    walkLExpr f op
    walkLExpr f right
-- negation (-a)
walkExpr f loc (NegApp expr _) = brace f loc "NegApp" $ walkLExpr f expr
-- (expr)
walkExpr f loc (HsPar expr) = brace f loc "HsPar" $ walkLExpr f expr
-- (1+)
walkExpr f loc (SectionL expr op) = brace f loc "SectionL" $ do
    walkLExpr f expr
    walkLExpr f op
-- (+1)
walkExpr f loc (SectionR op expr) = brace f loc "SectionR" $ do
    walkLExpr f op
    walkLExpr f expr
-- case expression
walkExpr f loc (HsCase expr mg) = brace f loc "HsCase" $ do
    walkLExpr f expr
    walkMatchGroup f Nothing mg
-- if expression
#if __GLASGOW_HASKELL__ >= 700
walkExpr f loc (HsIf _ expr ethen eelse) =
#else
walkExpr f loc (HsIf expr ethen eelse) =
#endif
    brace f loc "HsIf" $ do
        walkLExpr f expr
        walkLExpr f ethen
        walkLExpr f eelse
-- let expression
walkExpr f loc (HsLet locals expr) = brace f loc "HsLet" $ do
    walkLocals f locals
    walkLExpr f expr
-- do expression (incl. list comprehensions)
walkExpr f loc (HsDo _ stmts expr _) = brace f loc "HsDo" $ do
    mapM_ (walkLStmt f) stmts
    walkLExpr f expr
-- list [a,b,c]
walkExpr f loc (ExplicitList _ vals) = brace f loc "ExplicitList" $ mapM_ (walkLExpr f) vals
-- parallel array [:a,b,c:]
walkExpr f loc (ExplicitPArr _ vals) = brace f loc "ExplicitPArr" $ mapM_ (walkLExpr f) vals
-- tuple (a, b, c)
walkExpr f loc (ExplicitTuple args _) = brace f loc "ExplicitTuple" $ mapM_ (walkLExpr f) (concatMap toExpr args)
    where toExpr (Present expr) = [expr]
          toExpr _ = []
-- record constructor { f1=e1, f2=e2 }
walkExpr f loc (RecordCon con _ binds) = brace f loc "RecordCon" $ do
    walkId f con WCon
    walkRecord f binds
-- record update r { f1=e1, f2=e2 }
walkExpr f loc (RecordUpd expr binds _ _ _) = brace f loc "RecordUpd" $ do
    walkLExpr f expr
    walkRecord f binds
-- expr::Type
walkExpr f loc (ExprWithTySig expr typ) = brace f loc "ExprWithTySig" $ do
    walkLExpr f expr
    walkLType f typ
-- expr::Type after typecheck
walkExpr f loc (ExprWithTySigOut expr _) = brace f loc "ExprWithTySigOut" $ walkLExpr f expr
-- list [a..b], [a..], [a,b..], [a,b..c]
walkExpr f loc (ArithSeq _ si) = brace f loc "ArithSeq" $ walkSeq f si
-- parallel array [:a..b:] or [:a,b..c:]
walkExpr f loc (PArrSeq _ si) = brace f loc "PArrSeq" $ walkSeq f si
-- SCC pragma
walkExpr f loc (HsSCC _ expr) = brace f loc "HsSCC" $ walkLExpr f expr
-- core annotation
walkExpr f loc (HsCoreAnn _ expr) = brace f loc "HsCoreAnn" $ walkLExpr f expr
-- Template Haskell:
walkExpr f loc (HsBracket _) = brace f loc "HsBracket" $ return ()
walkExpr f loc (HsBracketOut _ _) = brace f loc "HsBracketOut" $ return ()
walkExpr f loc (HsSpliceE _) = brace f loc "HsSpliceE" $ return ()
walkExpr f loc (HsQuasiQuoteE _) = brace f loc "HsQuasiQuoteE" $ return ()
-- Arrows:
walkExpr f loc (HsProc _ _) = brace f loc "HsProc" $ return ()
walkExpr f loc (HsArrApp _ _ _ _ _) = brace f loc "HsArrApp" $ return ()
walkExpr f loc (HsArrForm _ _ _) = brace f loc "HsArrForm" $ return ()
-- Hpc support:
walkExpr f loc (HsTick _ _ _) = brace f loc "HsTick" $ return ()
walkExpr f loc (HsBinTick _ _ _) = brace f loc "HsBinTick" $ return ()
walkExpr f loc (HsTickPragma _ _) = brace f loc "HsTickPragma" $ return ()
-- parser temporary:
walkExpr f loc EWildPat = brace f loc "EWildPat" $ return ()
walkExpr f loc (EAsPat _ _) = brace f loc "EAsPat" $ return ()
walkExpr f loc (EViewPat _ _) = brace f loc "EViewPat" $ return ()
walkExpr f loc (ELazyPat _) = brace f loc "ELazyPat" $ return ()
walkExpr f loc (HsType typ) = brace f loc "HsType" $ walkLType f typ
-- ???
walkExpr f loc (HsWrap _wrapper expr) = brace f loc "HsWrap" $ walkExpr f loc expr

walkSeq:: (Monad m) => Callback a m -> ArithSeqInfo a -> m ()
walkSeq f (From from) = walkLExpr f from
walkSeq f (FromThen from thn) = do
    walkLExpr f from
    walkLExpr f thn
walkSeq f (FromTo from to) = do
    walkLExpr f from
    walkLExpr f to
walkSeq f (FromThenTo from thn to) = do
    walkLExpr f from
    walkLExpr f thn
    walkLExpr f to

walkRecord :: (Monad m) => Callback a m -> HsRecordBinds a -> m ()
walkRecord f binds = mapM_ walkField (rec_flds binds)
    where walkField (HsRecField fld arg _) = do
              walkId f fld WVal
              walkLExpr f arg

----------------------------------------------------------------------------------------------
-- Declarations
----------------------------------------------------------------------------------------------

walkLBinds :: (Monad m) => Callback a m -> Maybe a -> LHsBinds a -> m ()
walkLBinds f outer lbinds = do
    mapBagM (walkLBind f outer) lbinds
    return ()

walkLBind :: (Monad m) => Callback a m -> Maybe a -> LHsBindLR a a -> m ()
walkLBind f outer lbind = walkValD f outer (getLoc lbind) (unLoc lbind)

-- Normal declarations
walkValD :: (Monad m) => Callback a m -> Maybe a -> SrcSpan -> HsBind a -> m ()
-- function declaration (including value declaration)
walkValD f outer loc (FunBind funId _ mg _ _ _) = brace f loc "FunBind" $ do
    walkId f funId WFunDecl
    walkMatchGroup f (case outer of { Nothing -> Just $ unLoc funId; outerFunc -> outerFunc }) mg
-- pattern declaration
walkValD f _ loc (PatBind lhs rhss _ _) = brace f loc "PatBind" $ do
    walkLPattern f lhs
    walkRHSs f rhss
-- ???
#if __GLASGOW_HASKELL__ >= 700
walkValD f _ loc (VarBind _ _ _) =
#else
walkValD f _ loc (VarBind _ _) =
#endif
     brace f loc "VarBind" $ return ()
{-
walkValD f _ loc (VarBind var expr) = brace f loc "VarBind" $ do
    walkId' f var loc WFunDecl
    walkLExpr f expr
-}
-- ???
#if __GLASGOW_HASKELL__ >= 700
walkValD f _ loc (AbsBinds _ _ exps _ binds) =
#else
walkValD f _ loc (AbsBinds _ _ exps binds) =
#endif
    brace f loc "AbsBinds" $ do
        mapM_ (\id -> walkId' f id loc WFunDecl2) ids
        walkLBinds f (listToMaybe ids) binds
        where ids = [x | (_, x, _, _) <- exps]


-- Type/class declarations
walkTyClD :: (Monad m) => Callback a m -> SrcSpan -> TyClDecl a -> m ()
-- foreign type
walkTyClD f loc (ForeignType name _) = brace f loc "ForeignType" $ walkId f name WTyDecl
-- type family declaration
walkTyClD f loc (TyFamily _ name _ _) = brace f loc "TyFamily" $ walkId f name WTyDecl
-- data type declaration
walkTyClD f loc (TyData _ _ name _ _ _ cons _) = brace f loc "TyData" $ do
    walkId f name WTyDecl
    mapM_ walkCons cons
    where walkCons lcon = brace f (getLoc lcon) "ConDecl" $ do
              walkId f cname WConDecl
              mapM_ (walkLType f) (hsConDeclArgTys details)
              where (ConDecl cname _ _ _ details _ _ _) = unLoc lcon -- todo
-- type synonym declaration
walkTyClD f loc (TySynonym name _ _ typ) = brace f loc "TySynonym" $ do
    walkId f name WTyDecl
    walkLType f typ
-- type class declaration
walkTyClD f loc (ClassDecl _ name _ _ sigs defs _ _) = brace f loc "ClassDecl" $ do
    walkId f name WTyDecl
    mapM_ (walkLSig f) sigs
    walkLBinds f Nothing defs


-- Instance declarations
walkInstD :: (Monad m) => Callback a m -> SrcSpan -> InstDecl a -> m ()
walkInstD f loc (InstDecl _ _ _ _) = brace f loc "InstDecl" $ return ()


-- Deriving declarations
walkDerivD :: (Monad m) => Callback a m -> SrcSpan -> DerivDecl a -> m ()
walkDerivD f loc (DerivDecl _) = brace f loc "DerivDecl" $ return ()


-- Fixity declarations
walkFixD :: (Monad m) => Callback a m -> SrcSpan -> FixitySig a -> m ()
walkFixD f loc _ = brace f loc "FixitySig" $ return ()


-- Signature declarations
walkLSig :: (Monad m) => Callback a m -> LSig a -> m ()
walkLSig f sig = walkSigD f (getLoc sig) (unLoc sig)

walkSigD :: (Monad m) => Callback a m -> SrcSpan -> Sig a -> m ()
-- type signature
walkSigD f loc (TypeSig name typ) = brace f loc "TypeSig" $ do
    walkId f name WSig
    walkLType f typ
-- fixity
walkSigD f loc (FixSig fixSig) = brace f loc "FixSig" $ walkFixD f loc fixSig
-- inline pragmas
walkSigD f loc (InlineSig _ _) = brace f loc "InlineSig" $ return ()
-- specialisation pragma
walkSigD f loc (SpecSig _ _ _) = brace f loc "SpecSig" $ return ()
-- specialisation pragma for instance declaration
walkSigD f loc (SpecInstSig typ) = brace f loc "SpecInstSig" $ walkLType f typ
-- ???
walkSigD f loc (IdSig _) = brace f loc "IdSig" $ return ()


-- Default instance declarations
walkDefD :: (Monad m) => Callback a m -> SrcSpan -> DefaultDecl a -> m ()
walkDefD f loc (DefaultDecl _) = brace f loc "DefaultDecl" $ return ()


-- Foreign declarations
walkForD :: (Monad m) => Callback a m -> SrcSpan -> ForeignDecl a -> m ()
walkForD f loc (ForeignImport _ _ _) = brace f loc "ForeignImport" $ return ()
walkForD f loc (ForeignExport _ _ _) = brace f loc "ForeignExport" $ return ()


-- Warnings
walkWarnD :: (Monad m) => Callback a m -> SrcSpan -> WarnDecl a -> m ()
walkWarnD _f _loc _ = return ()


-- Rules
walkRuleD :: (Monad m) => Callback a m -> SrcSpan -> RuleDecl a -> m ()
walkRuleD _f _loc _ = return ()


-- Splices
walkSpliceD :: (Monad m) => Callback a m -> SrcSpan -> SpliceDecl a -> m ()
walkSpliceD _f _loc _ = return ()


-- Annotations
walkAnnD :: (Monad m) => Callback a m -> SrcSpan -> AnnDecl a -> m ()
walkAnnD _f _loc _ = return ()


-- Docs
walkDocD :: (Monad m) => Callback a m -> SrcSpan -> DocDecl -> m ()
walkDocD _f _loc _ = return ()


-- Use to walk TypecheckedSource tree
walkDeclarations :: (Monad m) => Callback a m -> LHsBinds a -> m ()
walkDeclarations f lbinds = walkLBinds f Nothing lbinds

walk :: (Monad m) => Callback a m -> LHsDecl a -> m ()
walk f decl = walkDecl f (getLoc decl) (unLoc decl)

walkDecl :: (Monad m) => Callback a m -> SrcSpan -> HsDecl a -> m ()
walkDecl f loc (TyClD tyClD) = brace f loc "TyClD" $ walkTyClD f loc tyClD
walkDecl f loc (InstD instD) = brace f loc "InstD" $ walkInstD f loc instD
walkDecl f loc (DerivD derivD) = brace f loc "DerivD" $ walkDerivD f loc derivD
walkDecl f loc (ValD valD) = brace f loc "ValD" $ walkValD f Nothing loc valD
walkDecl f loc (SigD sigD) = brace f loc "SigD" $ walkSigD f loc sigD
walkDecl f loc (DefD defD) = brace f loc "DefD" $ walkDefD f loc defD
walkDecl f loc (ForD forD) = brace f loc "ForD" $ walkForD f loc forD
walkDecl f loc (WarningD warnD) = brace f loc "WarningD" $ walkWarnD f loc warnD
walkDecl f loc (RuleD ruleD) = brace f loc "RuleD" $ walkRuleD f loc ruleD
walkDecl f loc (SpliceD spliceD) = brace f loc "SpliceD" $ walkSpliceD f loc spliceD
walkDecl f loc (AnnD annD) = brace f loc "AnnD" $ walkAnnD f loc annD
walkDecl f loc (DocD docD) = brace f loc "DocD" $ walkDocD f loc docD
#if __GLASGOW_HASKELL__ >= 700
walkDecl f loc (QuasiQuoteD _) = brace f loc "QuasiQuote" $ return ()
#endif


walkGroup :: (Monad m) => Callback a m -> HsGroup a -> m ()
walkGroup f (HsGroup valds tyclds instds derivds fixds defds fords warnds annds ruleds docs) = do
    walkBinds f Nothing valds
#if __GLASGOW_HASKELL__ >= 700
    mapM_ (walkLoc f walkTyClD) (concat tyclds)
#else
    mapM_ (walkLoc f walkTyClD) tyclds
#endif
    mapM_ (walkLoc f walkInstD) instds
    mapM_ (walkLoc f walkDerivD) derivds
    mapM_ (walkLoc f walkFixD) fixds
    mapM_ (walkLoc f walkDefD) defds
    mapM_ (walkLoc f walkForD) fords
    mapM_ (walkLoc f walkWarnD) warnds
    mapM_ (walkLoc f walkAnnD) annds
    mapM_ (walkLoc f walkRuleD) ruleds
    mapM_ (walkLoc f walkDocD) docs


walkModName :: (Monad m) => Callback a m -> Located ModuleName -> WhereMod -> m ()
walkModName f name definition = (modName f) (unLoc name) (getLoc name) definition

walkImport :: (Monad m) => Callback a m -> LImportDecl a -> m ()
walkImport f imp = when isRealImport $ brace f loc "Import" $ walkModName f (ideclName imp') WMImport
    where imp' = unLoc imp
          loc = getLoc imp
          isRealImport = isGoodSrcSpan loc

-- Use to walk ParsedSource tree
walkModule :: (Monad m) => Callback RdrName m -> ParsedSource -> m ()
walkModule f src = brace f (getLoc src) "Module" $ do
    let md = unLoc src
    case hsmodName md of
        (Just name) -> do
            walkModName f name WMModule
            walkName name
        _ -> return ()
    case hsmodExports md of
        (Just lies) -> mapM_ walkLIE lies
        _ -> return ()
    mapM_ (walkImport f) (hsmodImports md)
    mapM_ (walk f) (hsmodDecls md)
    where
        walkName name = brace f (getLoc name) "ModuleName" $ return ()
        walkLIE lie = brace f (getLoc lie) "Export" $ return ()

-- Use to walk RenamedSource tree
walkRenamed :: (Monad m) => Callback Name m -> RenamedSource -> m ()
walkRenamed f (group, imps, _, _) = do
    mapM_ (walkImport f) imps
    walkGroup f group
