module Yi.Buffer.Run
  ( runBuffer
  , runBufferFull
  , runBufferWithDefaultView
  ) where

import Prelude ()
import Yi.Prelude

-- import qualified Data.Map as M
-- import Data.Maybe (isNothing)
import Control.Monad.RWS.Strict (runRWS)

import Yi.Buffer.Basic
import Yi.Buffer.Implementation
import Yi.Buffer.Mode

-- | Execute a @BufferM@ value on a given buffer and window.  The new state of
-- the buffer is returned alongside the result of the computation.
runBuffer :: BufferView -> FBuffer -> BufferM a -> (a, FBuffer)
runBuffer bv b f =
    let (a, _, b') = runBufferFull bv b f
    in (a, b')

runBufferFull :: BufferView -> FBuffer -> BufferM a -> (a, [Update], FBuffer)
runBufferFull bv b f =
    let (a, b', updates) = runRWS (fromBufferM f') () b
        f' = setCurrentViewB bv >> f
    in (a, updates, pendingUpdatesA ^: (++ fmap TextUpdate updates) $ b')

runBufferWithDefaultView :: FBuffer -> BufferM a -> (a, FBuffer)
runBufferWithDefaultView bv b = error "runBufferWithDefaultView not implemented"
