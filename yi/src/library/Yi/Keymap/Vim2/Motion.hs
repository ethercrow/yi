module Yi.Keymap.Vim2.Motion
    ( Move(..)
    , CountedMove(..)
    , stringToMove 
    , regionOfMoveB
    , changeMoveStyle
    ) where

-- TODO:
--
-- respecting wrap in gj, g0, etc
--
-- gm, go
-- ]], [[, [], ][
-- [(, [{, ]), ]}
-- ]m, ]M, [m, [M
-- [#, ]#
-- [*, [/, ]*, ]/
-- H, M, L
--
-- Traversing changelist

-- TODO:
-- from vim help:
--
-- Special case: "cw" and "cW" are treated like "ce" and "cE" if the cursor is
-- on a non-blank.  This is because "cw" is interpreted as change-word, and a
-- word does not include the following white space.  {Vi: "cw" when on a blank
-- followed by other blanks changes only the first blank; this is probably a
-- bug, because "dw" deletes all the blanks}
--
-- Another special case: When using the "w" motion in combination with an
-- operator and the last word moved over is at the end of a line, the end of
-- that word becomes the end of the operated text, not the first word in the
-- next line.
--
-- The original Vi implementation of "e" is buggy.  For example, the "e" command
-- will stop on the first character of a line if the previous line was empty.
-- But when you use "2e" this does not happen.  In Vim "ee" and "2e" are the
-- same, which is more logical.  However, this causes a small incompatibility
-- between Vi and Vim.

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)

import Data.Maybe (fromMaybe)
import Data.Tuple (uncurry)

import Yi.Buffer
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StyledRegion

data Move = Move {
    moveStyle :: !RegionStyle
  , moveIsJump :: !Bool
  , moveAction :: Maybe Int -> BufferM ()
  }

data CountedMove = CountedMove !(Maybe Int) !Move

stringToMove :: String -> MatchResult Move
stringToMove s = lookupMove s
                 <|> matchGotoCharMove s
                 <|> matchGotoMarkMove s

lookupMove :: String -> MatchResult Move
lookupMove s = findMoveWithStyle Exclusive exclusiveMotions
           <|> findMoveWithStyle Inclusive inclusiveMotions
           <|> findMoveWithStyle LineWise linewiseMotions
    where findMoveWithStyle style choices = fmap (uncurry (Move style))
                                            (lookupBestMatch s (fmap regroup choices))
          regroup (a, b, c) = (a, (b, c))

changeMoveStyle :: (RegionStyle -> RegionStyle) -> Move -> Move
changeMoveStyle smod (Move s j m) = Move (smod s) j m

instance Functor ((,,) a b) where
    fmap f (a, b, c) = (a, b, f c)

-- Linewise motions which treat no count as being the same as a count of 1.
linewiseMotions :: [(String, Bool, Maybe Int -> BufferM ())]
linewiseMotions = fmap withDefaultCount
    [ ("j", False, discard . lineMoveRel)
    , ("k", False, discard . lineMoveRel . negate)
    , ("<Down>", False, discard . lineMoveRel)
    , ("<Up>", False, discard . lineMoveRel . negate)
    , ("-", False, const firstNonSpaceB <=< discard . lineMoveRel . negate)
    , ("+", False, const firstNonSpaceB <=< discard . lineMoveRel)
    , ("_", False, \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                firstNonSpaceB)
    , ("gg", True, discard . gotoLn) -- TODO: save column
    ]
    ++ [("G", True, gotoXOrEOF)]

-- Exclusive motions which treat no count as being the same as a count of 1.
exclusiveMotions :: [(String, Bool, Maybe Int -> BufferM ())]
exclusiveMotions = fmap withDefaultCount
    [ ("h", False, moveXorSol)
    , ("l", False, moveXorEol)
    , ("<Left>", False, moveXorSol)
    , ("<Right>", False, moveXorEol)
    , ("w", False, moveForwardB unitViWord)
    , ("W", False, moveForwardB unitViWORD)
    , ("b", False, moveBackwardB unitViWord)
    , ("B", False, moveBackwardB unitViWORD)
    , ("^", False, const firstNonSpaceB)
    , ("g^", False, const firstNonSpaceB) -- TODO: respect wrapping
    , ("g0", False, const moveToSol) -- TODO: respect wrapping
    , ("<Home>", False, const moveToSol)
    -- "0" sort of belongs here, but is currently handled as a special case in some modes
    , ("|", False, \n -> moveToSol >> moveXorEol (n - 1))
    , ("(", True, moveBackwardB unitSentence)
    , (")", True, moveForwardB unitSentence)
    , ("{", True, moveBackwardB unitEmacsParagraph)
    , ("}", True, moveForwardB unitEmacsParagraph)
    ]

-- Inclusive motions which treat no count as being the same as a count of 1.
inclusiveMotions :: [(String, Bool, Maybe Int -> BufferM ())]
inclusiveMotions = fmap (\(key, action) -> (key, False, action . fromMaybe 1))
    [
    -- Word motions
      ("e", repeat $ genMoveB unitViWord (Forward, InsideBound) Forward)
    , ("E", repeat $ genMoveB unitViWORD (Forward, InsideBound) Forward)
    , ("ge", repeat $ genMoveB unitViWord (Forward, InsideBound) Backward)
    , ("gE", repeat $ genMoveB unitViWORD (Forward, InsideBound) Backward)

    -- Intraline stuff
    , ("g$", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                moveToEol)
    , ("<End>", const $ moveToEol >> leftOnEol)
    , ("$", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                moveToEol
                leftOnEol)
    , ("g_", \n -> do
                when (n > 1) $ discard $ lineMoveRel (n - 1)
                lastNonSpaceB)
    ]
    ++
    [("%", True,
        \maybeCount -> case maybeCount of
            Nothing -> findMatchingPairB
            Just percent -> movePercentageFileB percent)
    ]

repeat :: BufferM () -> Int -> BufferM ()
repeat = flip replicateM_

regionOfMoveB :: CountedMove -> BufferM StyledRegion
regionOfMoveB = normalizeRegion <=< regionOfMoveB'

regionOfMoveB' :: CountedMove -> BufferM StyledRegion
regionOfMoveB' (CountedMove n (Move style _isJump move)) = do
    region <- mkRegion <$> pointB <*> destinationOfMoveB
        (move n >> when (style == Inclusive) leftOnEol)
    return $! StyledRegion style region

moveForwardB, moveBackwardB :: TextUnit -> Int -> BufferM ()
moveForwardB unit = repeat $ genMoveB unit (Backward,InsideBound) Forward
moveBackwardB unit = repeat $ moveB unit Backward

gotoXOrEOF :: Maybe Int -> BufferM ()
gotoXOrEOF n = case n of
    Nothing -> botB >> moveToSol
    Just n' -> gotoLn n' >> moveToSol

withDefaultCount :: (String, Bool, Int -> BufferM ()) -> (String, Bool, Maybe Int -> BufferM ())
withDefaultCount = fmap (. fromMaybe 1)

matchGotoMarkMove :: String -> MatchResult Move
matchGotoMarkMove (m:_) | m `notElem` "'`" = NoMatch
matchGotoMarkMove (_:[]) = PartialMatch
matchGotoMarkMove (m:c:[]) = WholeMatch $ Move style True action
    where style = if m == '`' then Inclusive else LineWise
          action _mcount = do
              mmark <- mayGetMarkB [c]
              case mmark of
                  Nothing -> fail $ "Mark " ++ show c ++ " not set"
                  Just mark -> moveTo =<< getMarkPointB mark
matchGotoMarkMove _ = NoMatch

matchGotoCharMove :: String -> MatchResult Move
matchGotoCharMove (m:[]) | m `elem` "fFtT" = PartialMatch
matchGotoCharMove (m:c:[]) | m `elem` "fFtT" = WholeMatch $ Move style False action
    where (dir, style, move) =
              case m of
                  'f' -> (Forward, Inclusive, nextCInLineInc c)
                  't' -> (Forward, Inclusive, nextCInLineExc c)
                  'F' -> (Backward, Exclusive, prevCInLineInc c)
                  'T' -> (Backward, Exclusive, prevCInLineExc c)
                  _ -> error "can't happen"
          action mcount = do
                  let count = fromMaybe 1 mcount
                  p0 <- pointB
                  replicateM_ (count - 1) $ do
                      move
                      moveB Character dir
                  p1 <- pointB
                  move
                  p2 <- pointB
                  when (p1 == p2) $ moveTo p0
matchGotoCharMove _ = NoMatch
