{-# LANGUAGE ExplicitForAll #-}

module Yi.Buffer.Misc
  ( deleteN
  , destinationOfMoveB
  , elemsB
  , insertCharWithBelowB
  , insertCharWithAboveB
  , lineCountB
  , lineDown
  , lineMoveRel
  , lineUp
  , markLines
  , moveToColB
  , moveToLineColB
  , movingToPrefCol
  , pointAfterCursorB
  , pointAt
  , pointOfLineColB
  , replaceCharB
  , replaceCharWithBelowB
  , replaceCharWithAboveB
  , savingExcursionB
  , savingPointB
  , savingPrefCol
  , withEveryLineB
  ) where

import Prelude ()
import Yi.Prelude

import Data.Maybe
import Data.List (length, scanl, takeWhile, zip)

import Yi.Buffer.Basic
import Yi.Buffer.Mode

pointAfterCursorB :: Point -> BufferM Point
pointAfterCursorB p = pointAt $ do
  moveTo p
  rightB

-- | What would be the point after doing the given action?
-- The argument must not modify the buffer.
destinationOfMoveB :: BufferM a -> BufferM Point
destinationOfMoveB f = savingPointB (f >> pointB)

withEveryLineB :: BufferM () -> BufferM ()
withEveryLineB action = savingPointB $ do
  lineCount <- lineCountB
  forM_ [1 .. lineCount] $ \l -> do
    discard $ gotoLn l
    action

pointAt :: forall a. BufferM a -> BufferM Point
pointAt f = savingPointB (f *> pointB)

pointOfLineColB :: Int -> Int -> BufferM Point
pointOfLineColB line col = savingPointB $ moveToLineColB line col >> pointB

-- | perform a @BufferM a@, and return to the current point. (by using a mark)
savingExcursionB :: BufferM a -> BufferM a
savingExcursionB f = do
    m <- getMarkB Nothing
    res <- f
    moveTo =<< getMarkPointB m
    return res

-- | Return line numbers of marks
markLines :: BufferM (MarkSet Int)
markLines = mapM getLn =<< askMarks
        where getLn m = getMarkPointB m >>= lineOf

replaceCharWithBelowB :: BufferM ()
replaceCharWithBelowB = replaceCharWithVerticalOffset 1

replaceCharWithAboveB :: BufferM ()
replaceCharWithAboveB = replaceCharWithVerticalOffset (-1)

insertCharWithBelowB :: BufferM ()
insertCharWithBelowB = maybe (return ()) insertB =<< maybeCharBelowB

insertCharWithAboveB :: BufferM ()
insertCharWithAboveB = maybe (return ()) insertB =<< maybeCharAboveB

replaceCharWithVerticalOffset :: Int -> BufferM ()
replaceCharWithVerticalOffset offset =
    maybe (return ()) replaceCharB =<< maybeCharWithVerticalOffset offset

maybeCharBelowB :: BufferM (Maybe Char)
maybeCharBelowB = maybeCharWithVerticalOffset 1

maybeCharAboveB :: BufferM (Maybe Char)
maybeCharAboveB = maybeCharWithVerticalOffset (-1)

maybeCharWithVerticalOffset :: Int -> BufferM (Maybe Char)
maybeCharWithVerticalOffset offset = savingPointB $ do
    l0 <- curLn
    c0 <- curCol
    discard $ lineMoveRel offset
    l1 <- curLn
    c1 <- curCol
    curChar <- readB
    if c0 == c1 && l0 + offset == l1 && curChar `notElem` "\n\0"
    then return $ Just curChar
    else return Nothing

replaceCharB :: Char -> BufferM ()
replaceCharB c = do
    deleteN 1
    insertB c
    leftB

-- | Delete @n@ characters forward from the current point
deleteN :: Int -> BufferM ()
deleteN n = pointB >>= deleteNAt Forward n

-- | perform an @BufferM a@, and return to the current point
savingPointB :: BufferM a -> BufferM a
savingPointB f = savingPrefCol $ do
  p <- pointB
  res <- f
  moveTo p
  return res

lineCountB :: BufferM Int
lineCountB = lineOf =<< lastB

-- | Return the contents of the buffer as a list
elemsB :: BufferM String
elemsB = nelemsB maxBound (Point 0)

-- | Move point up one line
lineUp :: BufferM ()
lineUp = lineMoveRel (-1) >> return ()

-- | Move point down one line
lineDown :: BufferM ()
lineDown = lineMoveRel 1 >> return ()

-- | Move point down by @n@ lines. @n@ can be negative.
-- Returns the actual difference in lines which we moved which
-- may be negative if the requested line difference is negative.
lineMoveRel :: Int -> BufferM Int
lineMoveRel = movingToPrefCol . gotoLnFrom

movingToPrefCol :: BufferM a -> BufferM a
movingToPrefCol f = do
  prefCol <- getPrefCol
  targetCol <- maybe curCol return prefCol
  r <- f
  moveToColB targetCol
  setPrefCol $ Just targetCol
  return r

moveToColB :: Int -> BufferM ()
moveToColB targetCol = do
  solPnt <- solPointB
  chrs <- nelemsB maxBound solPnt -- get all chars in the buffer, lazily.
  let cols = scanl colMove 0 chrs    -- columns corresponding to the char
      toSkip = takeWhile (\(char,col) -> char /= '\n' && col < targetCol) (zip chrs cols)
  moveTo $ solPnt +~ fromIntegral (length toSkip)

moveToLineColB :: Int -> Int -> BufferM ()
moveToLineColB line col = gotoLn line >> moveToColB col

savingPrefCol :: BufferM a -> BufferM a
savingPrefCol f = do
  pc <- getPrefCol
  result <- f
  setPrefCol pc
  return result