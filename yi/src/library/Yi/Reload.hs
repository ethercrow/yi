module Yi.Reload (reload) where

import Control.Monad.State

import Config.Dyre.Relaunch

import Yi.Editor
import Yi.Keymap
import qualified Yi.UI.Common as UI

-- | "reloads" the configuration
--
-- Serializes the editor state and relaunches Yi using the serialized state.
-- The launch of Yi will result in recompilation of the user's custom yi. This, in effect, "reloads"
-- the configuration.
reload :: YiM ()
reload = do
    editor <- withEditor get
    withUI (`UI.end` False)
    liftIO $ relaunchWithBinaryState (Just editor) Nothing
