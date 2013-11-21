module Yi.Keymap.Vim2.Ex.Commands.GotoLine
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char (isDigit)

import Yi.Buffer
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common (pureExCommand)

parse :: String -> Maybe ExCommand
parse s = if and $ not (null s) : fmap isDigit s
    then let l = read s in
         Just $ pureExCommand {
             cmdAction = BufferA $ gotoLn l >> firstNonSpaceB
           , cmdShow = s
         }
    else Nothing