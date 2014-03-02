module Unbounds (detectUnbounds) where

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)

import GHC
import PrelNames (isUnboundName)
import MonadUtils

import HUtil
import Walker

detectUnbounds :: String -> Ghc ()
detectUnbounds srcFile = do
    mod     <- loadHsFile srcFile
    parsed  <- parseModule mod
    checked <- typecheckModule parsed
    let maybeRenamed = renamedSource checked
    when (isJust maybeRenamed) $ extractUnbounds $ fromJust maybeRenamed

extractUnbounds :: RenamedSource -> Ghc ()
extractUnbounds renamed = do
    let cb = defWalkCallback { name = extractUnboundName }
    walkRenamed cb renamed

extractUnboundName name _ _ =
    when (isUnboundName name) $ printUnbound name

printUnbound name = liftIO $ putStrLn $ "unbound!!!!!!!!!! " ++ toString name -- todo
