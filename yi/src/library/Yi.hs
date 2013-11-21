{-# LANGUAGE CPP #-}
-- Copyright (c) 2007,8 JP Bernardy
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- |
-- Facade of the Yi library, for use by confguration file. Just re-exports
-- a bunch of modules.
--
-- You should therefore:
--      import Yi
-- in your ~/.config/yi/ scripts
--

module Yi (
    module Export
  , module Yi.Mode.Haskell
  , module Yi.Mode.IReader
  ) where

import Data.Prototype as Export -- prototypes are mainly there for config; makes sense to export them.
import Yi.Config as Export
import Yi.Core as Export
import Yi.Boot as Export
import Yi.Dired as Export
import Yi.Eval as Export
import Yi.File as Export
import Yi.Config.Default as Export
import Yi.Search as Export
import Yi.Style as Export
import Yi.Style.Library as Export
import Yi.Misc as Export
import Yi.Mode.Haskell (ghciGet, ghciLoadBuffer)
import Yi.Mode.IReader (ireaderMode, ireadMode)
#ifdef SCION
import Yi.Scion as Export
#endif
