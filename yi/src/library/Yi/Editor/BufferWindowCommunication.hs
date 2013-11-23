module Yi.Editor.BufferWindowCommunication
    ( ScrollStyle (..)
    , downScreenB
    , downScreensB
    , lastActiveWindowForBuffer
    , pointInWindowB
    , scrollB
    , scrollScreensB
    , scrollToCursorB
    , snapInsB
    , upScreenB
    , upScreensB
    , vimScrollB
    , vimScrollByB
    , visibleRegionOfBufferInWindow
    ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)
import qualified Data.List.PointedList as PL
import qualified Data.HashMap.Strict as HM

import Yi.Buffer
import Yi.Editor.ScrollStyle
import Yi.Window

visibleRegionOfBufferInWindow :: FBuffer -> Window -> Region
visibleRegionOfBufferInWindow b w = undefined

pointedlistToList :: PL.PointedList a -> [a]
pointedlistToList xs = PL._focus xs : (PL.suffix xs ++ PL.reversedPrefix xs)

lastActiveWindowForBuffer :: BufferRef -> PL.PointedList Window -> Maybe Window
lastActiveWindowForBuffer br ws =
    find (\w -> br `elem` (bufkey w : bufAccessList w)) (pointedlistToList ws)

upScreensB :: Window -> Int -> BufferM ()
upScreensB w = scrollScreensB w . negate

downScreensB :: Window -> Int -> BufferM ()
downScreensB = scrollScreensB

-- | Scroll up 1 screen
upScreenB :: Window -> BufferM ()
upScreenB w = scrollScreensB w (-1)

-- | Scroll down 1 screen
downScreenB :: Window -> BufferM ()
downScreenB w = scrollScreensB w 1

-- | Scroll by n screens (negative for up)
scrollScreensB :: Window -> Int -> BufferM ()
scrollScreensB w n =
    scrollB $ n * max 0 (height w - 3) -- subtract some amount to get some overlap (emacs-like).

-- | Scroll according to function passed. The function takes the
-- | Window height in lines, its result is passed to scrollB
-- | (negative for up)
scrollByB :: Window -> (Int -> Int) -> Int -> BufferM ()
scrollByB w f n = scrollB $ n * f (height w)

-- | Same as scrollB, but also moves the cursor
vimScrollB :: Int -> BufferM ()
vimScrollB n = do scrollB n
                  discard $ lineMoveRel n

-- | Same as scrollByB, but also moves the cursor
vimScrollByB :: Window -> (Int -> Int) -> Int -> BufferM ()
vimScrollByB w f n = vimScrollB $ n * f (height w)

-- | Move to middle line in screen
scrollToCursorB :: Window -> BufferM ()
scrollToCursorB w = do
    f <- lineOf =<< getFromMarkPointB
    i <- curLn
    let h = height w
    let m = f + (h `div` 2)
    scrollB $ i - m

-- | Move cursor to the top of the screen
scrollCursorToTopB :: BufferM ()
scrollCursorToTopB = do
    f <- lineOf =<< getFromMarkPointB
    i <- curLn
    scrollB $ i - f

-- | Move cursor to the bottom of the screen
scrollCursorToBottomB :: Region -> BufferM ()
scrollCursorToBottomB r = do
    i <- curLn
    t <- lineOf (regionEnd r -~ 1)
    scrollB $ i - t

-- | Scroll by n lines.
scrollB :: Int -> BufferM ()
scrollB n = savingPointB $ do
    moveTo =<< getFromMarkPointB
    discard $ gotoLnFrom n
    setFromMarkPointB =<< pointB

-- | Move to @n@ lines down from top of screen
downFromTosB :: Int -> BufferM ()
downFromTosB n = do
  moveTo =<< getFromMarkPointB
  replicateM_ n lineDown

-- | Move to @n@ lines up from the bottom of the screen
upFromBosB :: Region -> Int -> BufferM ()
upFromBosB wr n = do
  moveTo (regionEnd wr -~ 1)
  moveToSol
  replicateM_ n lineUp

-- | Move to middle line in screen
middleB :: Window -> BufferM ()
middleB w = do
  moveTo =<< getFromMarkPointB
  replicateM_ (height w `div` 2) lineDown

pointInWindowB :: Region -> Point -> BufferM Bool
pointInWindowB wr p = return $! nearRegion p wr
          

-- | Move the point to inside the viewable region
snapInsB :: Region -> BufferM ()
snapInsB r = do
    p <- pointB
    moveTo $ max (regionStart r) $ min (regionEnd r) p
