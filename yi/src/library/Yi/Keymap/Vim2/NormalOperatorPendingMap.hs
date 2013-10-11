module Yi.Keymap.Vim2.NormalOperatorPendingMap
  ( defNormalOperatorPendingMap
  ) where

import Prelude ()
import Yi.Prelude hiding (op, to)

import Data.Char (isDigit)
import Data.List (isPrefixOf, drop)
import Data.Maybe (fromMaybe, fromJust)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.Operator
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.TextObject
import Yi.Keymap.Vim2.Utils

defNormalOperatorPendingMap :: [VimOperator] -> [VimBinding]
defNormalOperatorPendingMap operators = [textObject operators, escBinding]

textObject :: [VimOperator] -> VimBinding
textObject operators = VimBindingE prereq action
    where
        prereq _ vs = case vsMode vs of
                            NormalOperatorPending _ -> WholeMatch ()
                            _ -> NoMatch
        action evs = do
            currentState <- getDynamic

            let partial = vsTextObjectAccumulator currentState
                opChar = lastCharForOperator op
                op = fromJust $ stringToOperator operators opname
                (NormalOperatorPending opname) = vsMode currentState

            -- vim treats cw as ce
            let evs' = if opname == "c" &&
                           last evs == 'w' &&
                           (case parseOperand opChar (partial ++ evs) of
                               JustMove _ -> True
                               _ -> False)
                       then init evs ++ "e"
                       else evs
                operand = parseOperand opChar (partial ++ evs')

            case operand of
                NoOperand -> do
                    dropTextObjectAccumulatorE
                    resetCountE
                    switchModeE Normal
                    return Drop
                PartialOperand -> do
                    accumulateTextObjectEventE evs
                    return Continue
                _ -> do
                    count <- getCountE
                    dropTextObjectAccumulatorE
                    token <- case operand of
                        JustTextObject cto@(CountedTextObject n _) -> do
                            normalizeCountE (Just n)
                            operatorApplyToTextObjectE op 1 $
                                changeTextObjectCount (count * n) cto
                        JustMove (CountedMove n m) -> do
                            mcount <- getMaybeCountE
                            normalizeCountE n
                            region <- withBuffer0 $ regionOfMoveB $ CountedMove (maybeMult mcount n) m
                            operatorApplyToRegionE op 1 region
                        JustOperator n style -> do
                            normalizeCountE (Just n)
                            normalizedCount <- getCountE
                            region <- withBuffer0 $ regionForOperatorLineB normalizedCount style
                            curPoint <- withBuffer0 pointB
                            token <- operatorApplyToRegionE op 1 region
                            when (opname == "y") $
                                withBuffer0 $ moveTo curPoint
                            return token

                        _ -> error "can't happen"
                    resetCountE
                    return token

regionForOperatorLineB :: Int -> RegionStyle -> BufferM StyledRegion
regionForOperatorLineB n style = normalizeRegion =<< StyledRegion style <$> savingPointB (do
    current <- pointB
    if n == 1
    then do
        firstNonSpaceB
        p0 <- pointB
        return $! mkRegion p0 current
    else do
        discard $ lineMoveRel (n-2)
        moveToEol
        rightB
        firstNonSpaceB
        p1 <- pointB
        return $! mkRegion current p1)

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

data OperandParseResult = JustTextObject !CountedTextObject
                         | JustMove !CountedMove
                         | JustOperator !Int !RegionStyle -- ^ like dd and d2vd
                         | PartialOperand
                         | NoOperand

parseOperand :: EventString -> String -> OperandParseResult
parseOperand opChar s = parseCommand mcount styleMod opChar commandString
    where (mcount, styleModString, commandString) = splitCountModifierCommand s
          styleMod = case styleModString of
                        "" -> id
                        "V" -> const LineWise
                        "<C-v>" -> const Block
                        "v" -> \style -> case style of
                                            Exclusive -> Inclusive
                                            _ -> Exclusive
                        _ -> error "Can't happen"

parseCommand :: Maybe Int -> (RegionStyle -> RegionStyle)
             -> EventString -> String -> OperandParseResult
parseCommand _ _ _ "" = PartialOperand
parseCommand _ _ _ "i" = PartialOperand
parseCommand _ _ _ "a" = PartialOperand
parseCommand _ _ _ "g" = PartialOperand
parseCommand n sm o s | o == s = JustOperator (fromMaybe 1 n) (sm LineWise)
parseCommand n sm _ s =
    case stringToMove s of
        WholeMatch m -> JustMove $ CountedMove n $ changeMoveStyle sm m
        PartialMatch -> PartialOperand
        NoMatch -> case stringToTextObject s of
            Just to -> JustTextObject $ CountedTextObject (fromMaybe 1 n)
                                      $ changeTextObjectStyle sm to
            Nothing -> NoOperand


-- Parse event string that can go after operator
-- w -> (Nothing, "", "w")
-- 2w -> (Just 2, "", "w")
-- V2w -> (Just 2, "V", "w")
-- v2V3<C-v>w -> (Just 6, "<C-v>", "w")
-- vvvvvvvvvvvvvw -> (Nothing, "v", "w")
splitCountModifierCommand :: String -> (Maybe Int, String, String)
splitCountModifierCommand = go "" Nothing [""]
    where go ds count mods (h:t) | isDigit h = go (ds ++ [h]) count mods t
          go ds@(_:_) count mods s@(h:_) | not (isDigit h) = go [] (maybeMult count (Just (read ds))) mods s
          go [] count mods (h:t) | h `elem` "vV" = go [] count ([h]:mods) t
          go [] count mods s | "<C-v>" `isPrefixOf` s = go [] count ("<C-v>":mods) (drop 5 s)
          go [] count mods s = (count, head mods, s)
          go ds count mods [] = (maybeMult count (Just (read ds)), head mods, [])
          go (_:_) _ _ (_:_) = error "Can't happen because isDigit and not isDigit cover every case"
