{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving #-}

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
} deriving (Typeable)

emptyJumpList :: JumpList
emptyJumpList = JumpList [] (-1)

jumpBack :: Int -> Window -> Window
jumpBack n w | currentJumpIndex (jumpList w) - n < 0 = w
             | otherwise = w { jumpList = JumpList (jumps (jumpList w))
                                                   ((currentJumpIndex (jumpList w)) - n)
                             }

jumpForward :: Int -> Window -> Window
jumpForward n w | currentJumpIndex (jumpList w) + n >= length (jumps (jumpList w)) = w
                | otherwise = w { jumpList = JumpList (jumps (jumpList w))
                                                      ((currentJumpIndex (jumpList w)) + n)
                                }

addJump :: (FilePath, Point) -> Window -> Window
addJump jump w = w { jumpList = JumpList newJumps  newIndex }
    where oldIndex = currentJumpIndex (jumpList w)
          newJumps = (take (oldIndex + 1) (jumps (jumpList w)) ++ [jump])
          newIndex = length newJumps

currentJump :: Window -> Maybe (FilePath, Point)
currentJump w = case jumpList w of
    JumpList [] _ -> Nothing
    JumpList jumps index -> if index >= 0 && index < length jumps
                            then Just $ jumps !! index
                            else Nothing

