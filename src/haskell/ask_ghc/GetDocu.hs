module GetDocu (getDocu) where

#if 1

import Data.List (intersperse)
import qualified Data.Map (keys, lookup)
import Data.Graph.Inductive.Query.Monad ((><))
import System.FilePath
import FastString (unpackFS)

import Name
import SrcLoc
import HsBinds
import HsTypes
import HsDecls
import HsDoc
import Outputable

import HUtil

import Documentation.Haddock

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

argsDocToStr (SigD (TypeSig locName typeName)) = mono (showName (unLoc locName) ++ " :: ") ++ hsDeclToStr typeName
argsDocToStr _                                 = ""

showName name = show $ (pprOccName $ nameOccName name) defaultUserStyle

hsDeclToStr decl =
  let surroundMono str open close = mono open ++ str ++ mono close
      surroundMonoDec dec         = surroundMono (hsDeclToStr dec)
  in case unLoc decl of
          HsForAllTy _ _ _ dec -> hsDeclToStr dec
          HsTyVar name         -> mono $ showName name
          HsAppTy decl1 decl2  -> hsDeclToStr decl1 ++ " " ++ hsDeclToStr decl2
          HsFunTy arg rest     -> hsDeclToStr arg ++ mono " -> " ++ hsDeclToStr rest
          HsListTy dec         -> surroundMonoDec dec "[" "]"
          HsPArrTy dec         -> surroundMonoDec dec "[:" ":]"
          HsTupleTy _ decs     -> surroundMono (concat $ intersperse (mono ", ") $ map hsDeclToStr decs) "(" ")"
          HsOpTy _ _ _         -> "op" -- todo: ???
          HsParTy dec          -> surroundMonoDec dec "(" ")"
          HsNumTy _            -> "num" -- todo: ???
          HsPredTy _           -> "pred" -- todo: ???
          HsKindSig _ _        -> "kindsig" -- todo: compile with appropriate option?
          HsQuasiQuoteTy _     -> "quasi" -- todo: ???
          HsSpliceTy _ _ _     -> "splice" -- todo: ???
          HsDocTy lhsType name -> let (HsDocString str) = unLoc name
            in hsDeclToStr lhsType ++ " " ++ unpackFS str
          HsBangTy _ _         -> "bang" -- todo: ???
          HsRecTy _            -> "rec"
          HsCoreTy _           -> "core" -- todo: ???

mono s = "<tt>" ++ s ++ "</tt>"

docToStr :: Doc id -> String
docToStr d =
    let list listType l = concat ["<", listType, "><li>", concat (intersperse "<li>" $ map docToStr l), "</", listType, ">"]
        monoDoc         = mono . docToStr
        docUnlines      = concat . intersperse "<br>"
    in case d of
        DocEmpty            -> ""
        DocAppend d1 d2     -> docUnlines [docToStr d1, docToStr d2]
        DocString s         -> s
        DocParagraph par    -> docToStr par ++ "<br>"
        DocIdentifier ids   -> ""
          -- todo: make link to identifier
          -- todo: how to show ids?
        DocModule s         -> s -- todo: link to module
        DocEmphasis d       -> "<i>" ++ docToStr d ++ "</i>" -- todo: italic or bold?
        DocMonospaced d     -> monoDoc d
        DocUnorderedList l  -> list "ul" l
        DocOrderedList l    -> list "ol" l
        DocDefList l        -> concatMap (\(id, def) -> monoDoc id ++ "<blockquote>" ++ docToStr def ++ "</blockquote>") l
        DocCodeBlock b      -> monoDoc b -- todo: syntax highlighting?
        DocURL u            -> "<a href=" ++ u ++ ">" ++ u ++ "</a>" -- todo: ???
        DocPic s            -> s -- todo: ???
        DocAName s          -> s -- todo: ???
        DocExamples es      -> docUnlines $ map (\e -> mono (exampleExpression e) ++ "<br>" ++ docUnlines (exampleResult e)) es
-- todo: check if documentation is present

#else

getDocu :: String -> String -> String -> (Int, Int) -> IO ()
getDocu _ _ _ _ = return ()

#endif
