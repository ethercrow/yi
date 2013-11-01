module Yi.Keymap.Vim2.Ex.Types where

import Prelude ()
import Yi.Prelude

import Data.Maybe

import Yi.Keymap

data ExCommand = ExCommand {
    cmdComplete :: YiM [String]
  , cmdIsPure :: Bool
  , cmdAction :: Action
  , cmdAcceptsRange :: Bool
  , cmdShow :: String
}

instance Show ExCommand where
    show = cmdShow

data LineRangeBoundary
    = LRBMark !String
    | LRBLineNumber !Int
    | LRBEOF
    | LRBCurrentLine
    deriving (Eq, Show)

-- | like :'a,'b or :123,$
data LineRange = LineRange {
    lrBegin :: !LineRangeBoundary
  , lrEnd :: LineRangeBoundary
  } deriving (Eq, Show)

stringToExCommand :: [String -> Maybe ExCommand] -> String -> Maybe ExCommand
stringToExCommand parsers s = listToMaybe . mapMaybe ($ s) $ parsers
