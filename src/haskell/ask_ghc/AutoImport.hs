module AutoImport (autoImport) where

import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import GHC
import MonadUtils

import HUtil

inspect name module' = do -- todo: module name comparison!
    (Just mi)   <- getModuleInfo module'
    let exports  = modInfoExports mi
    let filtered = getModule name `mapMaybe` exports
    mapM_ printModExports filtered

autoImport name ghcPath srcFile = runGhc (Just ghcPath) $ do
    flg         <- getSessionDynFlags
    (flg, _, _) <- parseDynamicFlags flg (map noLoc [])
    setSessionDynFlags $ flg { hscTarget = HscNothing, ghcLink = NoLink }
    modules <- packageDbModules True
    mapM_ (inspect name) (filteredModules modules)
        where filteredModules = filter ((/= srcFile) . toString)    -- todo: exclude imported modules as well
                                                                    -- todo: currently, user code is not being seen anyway

getModule functionName module' =
    let fullNameStr = toString module'
    in  if functionName == fullNameStr || ('.' : functionName) `isSuffixOf` fullNameStr
        then Just fullNameStr
        else Nothing

printModExports = liftIO . putStrLn