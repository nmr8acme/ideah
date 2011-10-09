module GetDocu (getDocu) where

#if 1

import Data.List (intersperse)
import qualified Data.Map (keys, lookup)
import Data.Graph.Inductive.Query.Monad ((><))
import Control.Monad (when)
import System.FilePath

import Name
import SrcLoc

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
         Just name -> case Data.Map.lookup name ifaceMap of
                         Just (lhsDecl, (maybeDoc, fnArgsDoc), _) -> do
                            putStrLn newMsgIndicator
                            let name = unLoc lhsDecl
                            when (length (Data.Map.keys fnArgsDoc) > 0) $ print $ argsDocToStr name fnArgsDoc
                            case maybeDoc of
                                 Just doc -> do
                                  putStrLn $ docToStr doc
                                 Nothing  -> return ()
                         Nothing -> return ()
         Nothing       -> return ()

argsDocToStr _ _ = "argsDocToStr"

{-argsDocToStr (ValD (HsBind id _)) = 
argsDocToStr (TyClD (TyClDecl id)) = 
argsDocToStr (InstD (InstDecl id)) = 
argsDocToStr (DerivD (DerivDecl id) = 
argsDocToStr (SigD (Sig id)) = 
argsDocToStr (DefD (DefaultDecl id)) = 
argsDocToStr (ForD (ForeignDecl id)) = 
argsDocToStr (WarningD (WarnDecl id)) =  
argsDocToStr (AnnD (AnnDecl id)) = 
argsDocToStr (RuleD (RuleDecl id)) = 
argsDocToStr (SpliceD (SpliceDecl id)) = 
argsDocToStr (DocD DocDecl) = 
argsDocToStr (QuasiQuoteD (HsQuasiQuote id)) = -}

docToStr :: Doc id -> String
docToStr d =
    let list listType l = concat $ ["<", listType, "><li>", concat (intersperse "<li>" $ map docToStr l), "</", listType, ">"]
        mono s          = "<tt>" ++ s ++ "</tt>"
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

#else

getDocu :: String -> String -> String -> (Int, Int) -> IO ()
getDocu _ _ _ _ = return ()

#endif
