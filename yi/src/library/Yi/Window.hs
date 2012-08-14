{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

--
-- Copyright (c) 2008 JP Bernardy
--
--

module Yi.Window where

import qualified Prelude
import Yi.Prelude
import Data.Binary
import Data.List (length, take, (!!))
import Yi.Buffer.Basic (BufferRef, WindowRef, Point)
import Yi.Region (Region,emptyRegion)
import System.IO (FilePath)

------------------------------------------------------------------------
-- | A window onto a buffer.

data Window = Window {
                      isMini    :: !Bool   -- ^ regular or mini window?
                     ,bufkey    :: !BufferRef -- ^ the buffer this window opens to
                     ,bufAccessList :: ![BufferRef] -- ^ list of last accessed buffers (former bufKeys). Last accessed one is first element
                     ,height    :: Int    -- ^ height of the window (in number of screen lines displayed)
                     ,winRegion    :: Region -- ^ view area.
                                              -- note that the top point is also available as a buffer mark.
                     ,wkey      :: !WindowRef -- ^ identifier for the window (for UI sync)
                     -- This is required for accurate scrolling.
                     -- Scrolling depends on the actual number of buffer
                     -- lines displayed. Line wrapping changes that number
                     -- relative to the height so we can't use height for that
                     -- purpose.
                     ,actualLines :: Int-- ^ The actual number of buffer lines displayed. Taking into account line wrapping
                     ,jumpList :: !JumpList
                     }
        deriving (Typeable)

instance Binary Window where
    put (Window mini bk bl _h _rgn key lns _jmps) = put mini >> put bk >> put bl >> put key >> put lns
    get = Window <$> get <*> get <*> get
                   <*> return 0 <*> return emptyRegion
                   <*> get <*> get <*> return emptyJumpList


-- | Get the identification of a window.
winkey :: Window -> (Bool, BufferRef)
winkey w = (isMini w, bufkey w)

instance Show Window where
    show w = "Window to " ++ show (bufkey w) 
             -- ++ "{" ++ show (tospnt w) ++ "->" ++ show (bospnt w) ++ "}" 
             ++ "(" ++ show (height w) ++ ")"

instance Eq Window where
    (==) w1 w2 = wkey w1 == wkey w2

{-
-- | Is a given point within tospnt / bospnt?
pointInWindow :: Point -> Window -> Bool
pointInWindow point win = tospnt win <= point && point <= bospnt win
-}

-- | Return a "fake" window onto a buffer.
dummyWindow :: BufferRef -> Window
dummyWindow b = Window False b [] 0 emptyRegion initial 0 emptyJumpList

data JumpList = JumpList {
  jumps :: [(FilePath, Point)],
  currentJumpIndex :: Int -- possible values are in [-1 .. length jumps]
} deriving (Typeable, Eq, Show)

emptyJumpList :: JumpList
emptyJumpList = JumpList [] (-1)

-- | If window is at the top of jumplist (e.g. just after addJump)
--   'j' will be appended to jumplist so that user can jump forward
--   after jumping back
jumpBack :: (FilePath, Point) -> Int -> Window -> Window
jumpBack j n w | n <= 0 = w
               | oldJumps == [] = w
               | oldIndex - n < 0 = w
               | atJumpListTip w && (last oldJumps /= j) =
                   jumpBack j (n + 1) (addJump j w)
               | not (atJumpListTip w) && oldJumps !! oldIndex /= j =
                   jumpBack j (n + 1) (addJump j w)
               | otherwise = w { jumpList = JumpList oldJumps
                                                   (oldIndex - n)
                               }
    where oldIndex = currentJumpIndex (jumpList w)
          oldJumps = jumps (jumpList w)

jumpForward :: Int -> Window -> Window
jumpForward n w | n <= 0 = w
                | oldIndex + n >= length oldJumps = w
                | otherwise = w { jumpList = JumpList oldJumps
                                                      (oldIndex + n)
                                }
    where oldIndex = currentJumpIndex (jumpList w)
          oldJumps = jumps (jumpList w)

addJump :: (FilePath, Point) -> Window -> Window
addJump jump w = w { jumpList = JumpList newJumps (length newJumps) }
    where newJumps | oldJumps == [] = [jump]
                   | last oldJumps == jump = oldJumps
                   | otherwise = oldJumps ++ [jump]
          oldJumps = take (oldIndex + 1) (jumps (jumpList w))
          oldIndex = currentJumpIndex (jumpList w)

currentJump :: Window -> Maybe (FilePath, Point)
currentJump w = case jumpList w of
    JumpList [] _ -> Nothing
    JumpList js index -> if index >= 0 && index < length js
                            then Just $ js !! index
                            else Nothing

atJumpListTip :: Window -> Bool
atJumpListTip w = index == length js
  where index = currentJumpIndex $ jumpList w
        js    = jumps $ jumpList w


hasJumps :: Window -> Bool
hasJumps w = case jumps (jumpList w) of
    [] -> False
    _  -> True


