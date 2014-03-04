module Compile (compile) where

import Control.Monad (when)
import Data.Char
import Data.List (isPrefixOf, partition)
import System.FilePath
import System.Directory
import System.Info (os)

import GHC
import HscTypes
import ErrUtils
import Bag
import Outputable
import MonadUtils
import SrcLoc
import FastString

import HUtil
import DynFlags (tracingDynFlags)

compile outPath srcPath ghcPath compilerOptions files =
    let
        skipOut = null outPath

        options = compilerOptions ++ ["--make"] 
               ++ if skipOut then ["-i" ++ srcPath]
                             else ["-i" ++ outPath ++ ":" ++ srcPath, "-outputdir " ++ outPath]
        
        compileFile file = runGhc (Just ghcPath) (doWalk options skipOut [file])

        exeExtension = case os of
            "mingw32" -> "exe"
            _ -> ""

        outputExe srcFile = (exeFile, outPath </> relPath </> takeFileName exeFile)
            where exeFile = replaceExtension srcFile exeExtension
                  relPath = dropFileName $ makeRelative srcPath srcFile

        renameOutput srcFile = do
            let (exeFile, new) = outputExe srcFile
            exists <- doesFileExist exeFile
            when exists $ renameFile exeFile new

        clearOutput ext = do
            let file = outPath </> addExtension "Main" ext
            exists <- doesFileExist file
            when exists $ removeFile file

        needsRecompile srcFile = do
            let (_, exeFile) = outputExe srcFile
            exeExists <- doesFileExist exeFile
            if exeExists
                then do
                    srcModified <- getModificationTime srcFile
                    exeModified <- getModificationTime exeFile
                    return $ srcModified > exeModified
                else return True

        compileNonModule srcFile = 
            if skipOut 
                then compileFile srcFile
                else do
                    recompile <- needsRecompile srcFile
                    when recompile $ do
                        compileFile srcFile
                        clearOutput "o"
                        clearOutput "hi"
                        renameOutput srcFile

        (modules, nonModules) = partition (isUpper . head . takeBaseName) files
    in do
        mapM_ compileFile modules
        mapM_ compileNonModule nonModules

doWalk :: [String] -> Bool -> [String] -> Ghc ()
doWalk cmdFlags skipOut files = do
    setupFlags skipOut cmdFlags
    flg <- getSessionDynFlags
    setSessionDynFlags $ flg { log_action = output2 }
    mapM_ addTargetFile files
    load LoadAllTargets `gcatch` catcher
    return ()

output warn span shortMsg longMsg = do
    putStrLn newMsgIndicator
    putStrLn$ maybe "?" unpackFS (srcSpanFileName_maybe span)
    putStrLn $ (if warn then "W" else "E") ++ spanStr span
    putStrLn shortMsg
    putStrLn longMsg

msgStr :: PprStyle -> SDoc -> String
msgStr = sdocToStringStyled

output1 :: (MonadIO m) => ErrMsg -> m ()
output1 msg = do
    let span     = head $ errMsgSpans msg
        style    = mkErrStyle tracingDynFlags $ errMsgContext msg
        shortMsg = msgStr style $ errMsgShortDoc msg
        longMsg  = msgStr style $ errMsgExtraInfo msg
        isWarn   = "Warning:" `isPrefixOf` shortMsg
    liftIO $ output isWarn span shortMsg longMsg

output2 :: DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
output2 _ severity span style msg = do
    let isWarn  = case severity of
            SevWarning -> True
            SevInfo -> True
            _ -> False
        msgText = msgStr style msg
    output isWarn span msgText msgText

catcher :: SourceError -> Ghc SuccessFlag
catcher err = do
    mapBagM output1 $ srcErrorMessages err
    return Failed
