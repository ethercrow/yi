module Yi.Buffer.Run
  ( runBuffer
  , runBufferFull
  , runBufferDummyWindow
  ) where

import Prelude ()
import Yi.Prelude

import qualified Data.Map as M
import Data.Maybe (isNothing)
import Control.Monad.RWS.Strict (runRWS)

import Yi.Buffer.Implementation
import Yi.Buffer.Mode
import Yi.Window

-- | Execute a @BufferM@ value on a given buffer and window.  The new state of
-- the buffer is returned alongside the result of the computation.
runBuffer :: Window -> FBuffer -> BufferM a -> (a, FBuffer)
runBuffer w b f = 
    let (a, _, b') = runBufferFull w b f 
    in (a, b')


runBufferFull :: Window -> FBuffer -> BufferM a -> (a, [Update], FBuffer)
runBufferFull w b f = 
    let (a, b', updates) = runRWS (fromBufferM f') w b
        f' = do
            ms <- getMarks w
            when (isNothing ms) $ do
                -- this window has no marks for this buffer yet; have to create them.
                newMarkValues <- if wkey (b ^. lastActiveWindowA) == initial
                    then return
                        -- no previous window, create some marks from scratch.
                         MarkSet { insMark = MarkValue 0 Forward,
                                   selMark = MarkValue 0 Backward, -- sel
                                   fromMark = MarkValue 0 Backward } -- from
                    else do
                        Just mrks  <- getsA winMarksA (M.lookup $ wkey (b ^. lastActiveWindowA))
                        forM mrks getMarkValueB
                newMrks <- forM newMarkValues newMarkB
                modA winMarksA (M.insert (wkey w) newMrks)
            putA lastActiveWindowA w
            f
    in (a, updates, pendingUpdatesA ^: (++ fmap TextUpdate updates) $ b')

-- | Execute a @BufferM@ value on a given buffer, using a dummy window.  The new state of
-- the buffer is discarded.
runBufferDummyWindow :: FBuffer -> BufferM a -> a 
runBufferDummyWindow b = fst . runBuffer (dummyWindow $ bkey b) b

