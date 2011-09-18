module GetDocu (getDocu) where

import OccName
import Unique
import Name
import qualified Data.Map (keys, lookup)

import HUtil

import Documentation.Haddock

getDocu file function = do
  (iface : _) <- createInterfaces [] [file]
  let ifaceMap           = ifaceDeclMap iface
      ifaceKeys          = Data.Map.keys ifaceMap
      ifaceNamesWithKeys = zip (map (toString . nameOccName) ifaceKeys) ifaceKeys
      funOcc             = mkOccName srcDataName function
  case lookup function ifaceNamesWithKeys of
     Just name -> case Data.Map.lookup name ifaceMap of
                       Just (_, (maybeDoc, fnArgsDoc), _) -> case maybeDoc of
                           Just doc -> putStrLn $ docToStr doc
                           Nothing -> return ()
                       Nothing -> return ()
                           -- todo: fnArgsdoc!
     Nothing       -> putStrLn "no bla"

docToStr DocEmpty             = "empty"
docToStr (DocAppend d1 d2 )   = docToStr d1 ++ "\n" ++ docToStr d2
docToStr (DocString s)        = "docstring " ++ s
docToStr (DocParagraph par)   = docToStr par
docToStr (DocIdentifier _)    = "docid"
docToStr (DocModule s)        = "docmod " ++ s
docToStr (DocEmphasis _)      = "docemph"
docToStr (DocMonospaced _)    = "docmono"
docToStr (DocUnorderedList _) = "docunord"
docToStr (DocOrderedList _)   = "docord"
docToStr (DocDefList _)       = "docdef"
docToStr (DocCodeBlock _)     = "doccode"
docToStr (DocURL s)           = "docurl " ++ s
docToStr (DocPic s)           = "docpic " ++ s
docToStr (DocAName s)         = "docaname " ++ s
docToStr (DocExamples _)      = "docexamples"
