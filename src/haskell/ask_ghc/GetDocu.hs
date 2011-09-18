module GetDocu (getDocu) where

import Prelude hiding (lookup)

import GHC
import MonadUtils
import RdrName
import OccName
import Unique
import Name
import Data.Map (lookup)

import HUtil

import Documentation.Haddock

getDocu file function = do
  setupFlags True []
  buffer <- liftIO $ loadFile file
  result <- parseHsFile buffer file
  case result of
       Right parsed -> let 
            {-hsMod      = unLoc parsed
            modName    = case hsmodName hsMod of
                              Just name -> unLoc name
                              Nothing   -> mkModuleName "Main"-}
            funOcc     = mkOccName srcDataName function
            --rdrName = mkRdrQual modName funOcc
         in liftIO $ do
          (iface : _) <- createInterfaces [] [file]
          case lookup (mkFCallName (getUnique funOcc) function) (ifaceDeclMap iface) of
               Just declInfo -> putStrLn "bla"
               Nothing       -> putStrLn "no bla"
       Left (span, error) -> liftIO $ ioError $ userError $ spanStr span ++ ": " ++ error
