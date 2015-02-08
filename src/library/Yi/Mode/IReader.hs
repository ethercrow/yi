{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.IReader
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple text mode; it does very little besides define a comment
-- syntax. We have it as a separate mode so users can bind the
-- commands to this mode specifically.

module Yi.Mode.IReader where

import Data.Text ()
import Control.Lens
import Data.Char (intToDigit)
import Data.Foldable (asum)
import Yi.Buffer.Misc
import Yi.Editor
import Yi.IReader
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Modes (anyExtension, fundamentalMode)

abstract :: Mode syntax
abstract = fundamentalMode { modeApplies = anyExtension ["irtxt"]
                           , modeKeymap = topKeymapA %~ ikeys }
  where
    ikeys = important $ asum m
    m = [ metaCh '`' ?>>! saveAsNewArticle
        , metaCh '0' ?>>! deleteAndNextArticle
        ]
        ++ map (\x -> metaCh (intToDigit x) ?>>! saveAndNextArticle x) [1..9]

ireaderMode :: Mode syntax
ireaderMode = abstract { modeName = "interactive reading of text" }

ireadMode ::  YiM ()
ireadMode = do
  withCurrentBuffer $ setAnyMode $ AnyMode ireaderMode
  nextArticle
  printMsg "M-` new; M-0 delete; M-[1-9]: save w/higher priority"
