module Yi.Hdevtools
    ( printTypeOfCurrentWord
    , printType
    , checkCurrentFile
    ) where

import Prelude (lines)
import Data.List (take, filter)
import Yi
import Yi.Process (runProgCommand)

import Control.Monad.Trans (liftIO)

binaryName :: String
binaryName = "hdevtools"

printTypeOfCurrentWord :: YiM ()
printTypeOfCurrentWord = do
    identifier <- withBuffer $ regionOfB unitViWord >>= readRegionB
    printType identifier

printType :: String -> YiM ()
printType identifier = do
    currentFilePath <- withBuffer $ fmap bufInfoFileName bufInfoB
    (stdout, _stderr, _exitcode) <- liftIO $
        runProgCommand binaryName ["info", currentFilePath, identifier] 
    case stdout of
        [] -> return ()
        text -> withEditor $ printMsg $ sanitize $ head $ lines text

checkCurrentFile :: YiM ()
checkCurrentFile = do
    currentFilePath <- withBuffer $ fmap bufInfoFileName bufInfoB
    (stdout, _stderr, _exitcode) <- liftIO $
        runProgCommand binaryName ["check", currentFilePath] 
    case stdout of
        [] -> return ()
        text -> withEditor $ printMsg $ sanitize $ head $ lines text

sanitize :: String -> String
sanitize = take 80 . filter (/= '\t')
