{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | This module defines implementations of syntax-awareness drivers.

module Yi.Syntax.Driver
    ( mkHighlighter
    ) where

import Yi.Syntax (mkHighlighter
import Yi.Syntax.Tree
