module GetDocu (getDocu) where

#if __GLASGOW_HASKELL__ >= 700

import Data.List (intersperse)
import qualified Data.Map (keys, lookup)
import Data.Graph.Inductive.Query.Monad ((><))
import System.FilePath
import FastString (unpackFS)
import Control.Monad.State

import Name
import SrcLoc
import HsBinds
import HsTypes
import HsDecls
import HsDoc
import Outputable

import HUtil

import Documentation.Haddock

getDocu :: FilePath -> FilePath -> (Int, Int) -> FilePath -> IO ()
getDocu srcPath ghcPath loc modFile = do
    ifaces <- createInterfaces [ Flag_GhcLibDir ghcPath
                               , Flag_OptGhc ("-i " ++ srcPath)] [modFile]
    let (iface : _)       = filter (equalFilePath modFile . ifaceOrigFilename) ifaces
        ifaceMap          = ifaceDeclMap iface
        ifaceKeys         = Data.Map.keys ifaceMap
        ifaceLocs         = map nameSrcLoc ifaceKeys
        ifaceLocsWithKeys = zip (map (srcLocLine >< srcLocCol) (zip ifaceLocs ifaceLocs)) ifaceKeys
    case lookup loc ifaceLocsWithKeys of
         Just name ->
          case Data.Map.lookup name ifaceMap of
               Just (lhsDecl, (maybeDoc, _), _) -> do
                  putStrLn newMsgIndicator
                  putStrLn $ argsDocToStr $ unLoc lhsDecl
                  case maybeDoc of
                       Just doc -> do
                        putStrLn $ docToStr doc
                       Nothing  -> return ()
               Nothing -> return ()
         Nothing       -> return ()

argsDocToStr :: HsDecl Name -> String
argsDocToStr (SigD (TypeSig locName typeName)) = let (argDoc, hasArgDoc) = runState (hsDeclToStr typeName) False
  in if hasArgDoc then mono (showName (unLoc locName) ++ " :: " ++ argDoc) else ""
argsDocToStr _                                 = ""

showName name = show $ (pprOccName $ nameOccName name) defaultUserStyle

hsDeclToStr :: Located (HsType Name) -> State Bool String
hsDeclToStr ldecl = do
  let surroundMono str open close    = mono open ++ str ++ mono close
      surroundMonoDec dec open close = do
        dec' <- hsDeclToStr dec
        return $ surroundMono dec' open close
  case unLoc ldecl of
          HsForAllTy _ _ _ dec -> hsDeclToStr dec
          HsTyVar name         -> return $ mono $ showName name
          HsAppTy decl1 decl2  -> do
            decl1' <- hsDeclToStr decl1
            decl2' <- hsDeclToStr decl2
            return $ decl1' ++ " " ++ decl2'
          HsFunTy arg rest     -> do
            arg'  <- hsDeclToStr arg
            rest' <- hsDeclToStr rest
            return $ arg' ++ mono " -> " ++ rest'
          HsListTy dec         -> surroundMonoDec dec "[" "]"
          HsPArrTy dec         -> surroundMonoDec dec "[:" ":]"
          HsTupleTy _ decs     -> do
            decs' <- mapM hsDeclToStr decs
            return $ surroundMono (concat $ intersperse (mono ", ") decs') "(" ")"
          HsOpTy _ _ _         -> return "op" -- todo: ???
          HsParTy dec          -> surroundMonoDec dec "(" ")"
          HsNumTy _            -> return "num" -- todo: ???
          HsPredTy _           -> return "pred" -- todo: ???
          HsKindSig _ _        -> return "kindsig" -- todo: compile with appropriate option?
          HsQuasiQuoteTy _     -> return "quasi" -- todo: ???
          HsSpliceTy _ _ _     -> return "splice" -- todo: ???
          HsDocTy lhsType name -> do
            let HsDocString str = unLoc name
            put True
            lhsType' <- hsDeclToStr lhsType
            return $ lhsType' ++ " " ++ unpackFS str
          HsBangTy _ _         -> return "bang" -- todo: ???
          HsRecTy conFields    -> do
            fields <- mapM (\c -> do
              fldType <- hsDeclToStr $ cd_fld_type c
              return $ nl ++ monobold (showName $ unLoc $ cd_fld_name c)
                      ++ " :: " ++ fldType
                      ++ show ((ppr_mbDoc $ cd_fld_doc c) defaultUserStyle))
                conFields
            return $ concat fields -- todo: test?!
          HsCoreTy _           -> return "core" -- todo: ???

mono s = "<tt>" ++ s ++ "</tt>"

bold s = "<b>" ++ s ++ "</b>"

monobold = mono . bold

nl = "<br>"

docToStr :: Doc Name -> String
docToStr d =
    let list listType l = concat ["<", listType, "><li>", concat (intersperse "<li>" $ map docToStr l), "</", listType, ">"]
        monoDoc         = mono . docToStr
        docUnlines      = concat . intersperse nl
    in case d of
        DocEmpty            -> ""
        DocAppend d1 d2     -> docUnlines [docToStr d1, docToStr d2]
        DocString s         -> s
        DocParagraph par    -> docToStr par ++ nl
        DocIdentifier ids   -> monobold (concat (intersperse ", " $ map showName ids)) -- todo: why list of identifiers???
          -- todo: make link to identifier
          -- todo: how to show ids?
        DocModule s         -> monobold s -- todo: link to module
        DocEmphasis d       -> "<i>" ++ docToStr d ++ "</i>" -- todo: italic or bold?
        DocMonospaced d     -> monoDoc d
        DocUnorderedList l  -> list "ul" l
        DocOrderedList l    -> list "ol" l
        DocDefList l        -> concatMap (\(id, def) -> monoDoc id ++ "<blockquote>" ++ docToStr def ++ "</blockquote>") l
        DocCodeBlock b      -> monoDoc b -- todo: syntax highlighting?
        DocURL u            -> "<a href=" ++ u ++ ">" ++ u ++ "</a>" -- todo: ???
        DocPic s            -> s -- todo: ???
        DocAName s          -> s -- todo: ???
        DocExamples es      -> docUnlines $ map (\e -> mono (exampleExpression e) ++ nl ++ docUnlines (exampleResult e)) es

#else

getDocu :: FilePath -> FilePath -> (Int, Int) -> FilePath -> IO ()
getDocu _ _ _ _ = return ()

#endif
