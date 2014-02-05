module AutoImport (autoImport) where

import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import GHC
import MonadUtils

import HUtil

inspect name module' = do
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
        where filteredModules = filter ((/= srcFile) . toString)    -- todo: exclude imported modules as well, and do proper module comparison (not just based on string equality)
                                                                    -- todo: currently, user code is not seen anyway, so filterModules is useless

getModule functionName module' =
    let fullNameStr = toString module'
    in  if functionName == fullNameStr || ('.' : functionName) `isSuffixOf` fullNameStr
        then Just fullNameStr
        else Nothing

printModExports = liftIO . putStrLn