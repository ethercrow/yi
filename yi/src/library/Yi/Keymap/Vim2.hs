{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Yi.Keymap.Vim2
    ( keymapSet
    , mkKeymapSet
    , defModeMapProto
    , VimBinding (..)
    , ModeMap (..)
    , vimEval
    -- | for testing purposes
    , allBindings
    , defaultVimEval
    ) where

import Prelude ()
import Yi.Prelude

import Data.List (map, filter)
import Data.Prototype

import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Keymap.Keys (anyEvent)

import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.InsertMap
import Yi.Keymap.Vim2.NormalMap
import Yi.Keymap.Vim2.NormalGotoCharacterMap
import Yi.Keymap.Vim2.NormalOperatorPendingMap
import Yi.Keymap.Vim2.ReplaceMap
import Yi.Keymap.Vim2.ReplaceSingleCharMap
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.VisualMap

data ModeMap = ModeMap {
        vimKeymap :: Keymap,
        normalMap :: [VimBinding],
        normalGotoCharacterMap :: [VimBinding],
        normalOperatorPendingMap :: [VimBinding],
        insertMap :: [VimBinding],
        replaceSingleMap :: [VimBinding],
        replaceMap :: [VimBinding],
        visualMap :: [VimBinding]
    }

mkKeymapSet :: Proto ModeMap -> KeymapSet
mkKeymapSet = modelessKeymapSet . vimKeymap . extractValue

keymapSet :: KeymapSet
keymapSet = mkKeymapSet defModeMapProto

defModeMapProto :: Proto ModeMap
defModeMapProto = Proto template
    where template self = ModeMap {
                              vimKeymap = defVimKeymap self,
                              normalMap = defNormalMap,
                              normalGotoCharacterMap = defNormalGotoCharacterMap,
                              normalOperatorPendingMap = defNormalOperatorPendingMap,
                              insertMap = defInsertMap,
                              replaceSingleMap = defReplaceSingleMap,
                              replaceMap = defReplaceMap,
                              visualMap = defVisualMap
                          }

defVimKeymap :: ModeMap -> KeymapM ()
defVimKeymap mm = do
    e <- anyEvent
    write $ handleEvent mm e

handleEvent :: ModeMap -> Event -> YiM ()
handleEvent mm e = do
    currentState <- withEditor getDynamic
    let maybeBinding = find (isBindingApplicable e currentState) (allBindings mm)

    repeatToken <- case maybeBinding of
        Nothing -> fail $ "unhandled event " ++ show e ++ " at top level"
        Just (VimBindingY _ action) -> action e
        Just (VimBindingE _ action) -> withEditor $ action e

    withEditor $ do
        case repeatToken of
            Drop -> dropAccumulatorE
            Continue -> accumulateEventE e
            Finish -> accumulateEventE e >> flushAccumulatorIntoRepeatableActionE

        performEvalIfNecessary mm

allBindings :: ModeMap -> [VimBinding]
allBindings m = concat [ normalMap m
                       , normalGotoCharacterMap m
                       , normalOperatorPendingMap m
                       , insertMap m
                       , replaceSingleMap m
                       , replaceMap m
                       , visualMap m
                       ]

-- This is not in Yi.Keymap.Vim2.Eval to avoid circular dependency:
-- eval needs to know about bindings, which contains normal bindings,
-- which contains '.', which needs to eval things
-- So as a workaround '.' just saves a string that needs eval in VimState
-- and the actual evaluation happens in handleEvent
vimEval :: ModeMap -> String -> EditorM ()
vimEval mm s = sequence_ actions
        where actions = map (pureHandleEvent mm) $ parseEvents s

defaultVimEval :: String -> EditorM ()
defaultVimEval = vimEval $ extractValue defModeMapProto

pureHandleEvent :: ModeMap -> Event -> EditorM ()
pureHandleEvent mm event = do
    currentState <- getDynamic
    let maybeBinding = find (isBindingApplicable event currentState) (allPureBindings mm)
    case maybeBinding of
        Nothing -> fail $ "unhandled event " ++ show event ++ " in " ++ show (vsMode currentState)
        Just (VimBindingE _ action) -> do
            repeatToken <- withEditor $ action event
            case repeatToken of
                Drop -> dropAccumulatorE
                Continue -> accumulateEventE event
                Finish -> accumulateEventE event >> flushAccumulatorIntoRepeatableActionE
        Just (VimBindingY _ _) -> fail "Impure binding found"

    performEvalIfNecessary mm

performEvalIfNecessary :: ModeMap -> EditorM ()
performEvalIfNecessary mm = do
    stateAfterAction <- getDynamic

    -- see comment for 'vimEval' below
    modifyStateE $ \s -> s { vsStringToEval = "" }
    vimEval mm $ vsStringToEval stateAfterAction

allPureBindings :: ModeMap -> [VimBinding]
allPureBindings mm = filter isPure $ allBindings mm
    where isPure (VimBindingE _ _) = True
          isPure _ = False
