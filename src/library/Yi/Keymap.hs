{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 'Keymap', 'YiM' and 'Action's.


module Yi.Keymap
    ( Action(..)
    , emptyAction
    , Interact
    , KeymapM
    , Keymap
    , KeymapEndo
    , KeymapProcess
    , KeymapSet(..)
    , topKeymapA
    , insertKeymapA
    , extractTopKeymap
    , modelessKeymapSet
    , YiM(..)
    , withUI
    , unsafeWithEditor
    , readEditor
    , YiAction (..)
    , Yi(..)
    , IsRefreshNeeded(..)
    , YiVar(..)
    , write
    , withModeY

    -- * Lenses
    , yiSubprocessesA
    , yiEditorA
    , yiSubprocessIdSupplyA
    , yiConfigA
    , yiInputA
    , yiOutputA
    , yiUiA
    , yiVarA
    ) where

import           Control.Exception
import           Control.Monad.Reader hiding (mapM_)
import           Control.Monad.State hiding (mapM_)
import           Yi.Buffer
import qualified Yi.Editor as Editor
import qualified Yi.Interact as I
import           Yi.Monad
import           Yi.Types
import           Yi.UI.Common
import           Yi.Utils


-----------------------
-- Keymap basics

-- | @write a@ returns a keymap that just outputs the action @a@.
write :: (I.MonadInteract m Action ev, YiAction a x, Show x) => a -> m ()
write x = I.write (makeAction x)

--------------------------------
-- Uninteresting glue code

withUI :: (UI Editor -> IO a) -> YiM a
withUI = with yiUi

readEditor :: MonadEditor m => (Editor -> a) -> m a
readEditor f = withEditor (gets f)

-- -------------------------------------------

class YiAction a x | a -> x where
    makeAction :: Show x => a -> Action

instance YiAction (IO x) x where
    makeAction = YiA . io

instance YiAction (YiM x) x where
    makeAction = YiA

instance YiAction (EditorM x) x where
    makeAction = EditorA

instance YiAction (BufferM x) x where
    makeAction = BufferA

instance YiAction Action () where
    makeAction = id

makeLensesWithSuffix "A" ''KeymapSet

modelessKeymapSet :: Keymap -> KeymapSet
modelessKeymapSet k = KeymapSet
 { insertKeymap = k
 , topKeymap = k
 }

-- | @withModeY f@ runs @f@ on the current buffer's mode. As this runs in
-- the YiM monad, we're able to do more than with just 'withModeB' such as
-- prompt the user for something before running the action.
withModeY :: (forall syntax. Mode syntax -> YiM ()) -> YiM ()
withModeY f = do
   bufref <- gets Editor.currentBuffer
   mfbuf <- Editor.findBuffer bufref
   case mfbuf of
     Nothing -> return ()
     Just (FBuffer {bmode = m}) -> f m

makeLensesWithSuffix "A" ''YiVar
makeLensesWithSuffix "A" ''Yi
