{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell #-}
-- Copyright 2008 JP Bernardy
-- | Basic types useful everywhere we play with buffers.
module Yi.Buffer.Basic where

import Prelude (reverse)
import Yi.Prelude

import Data.Binary    
import Data.DeriveTH
import Data.Ix

import qualified Data.Rope as R

-- | Direction of movement inside a buffer
data Direction = Backward
               | Forward
                 deriving (Eq, Ord, Typeable, Show, Bounded, Enum)

$(derive makeBinary ''Direction)

reverseDir :: Direction -> Direction
reverseDir Forward = Backward
reverseDir Backward = Forward

-- | reverse if Backward
mayReverse :: Direction -> [a] -> [a]
mayReverse Forward = id
mayReverse Backward = reverse

-- | 'direction' is in the same style of 'maybe' or 'either' functions,
-- It takes one argument per direction (backward, then forward) and a
-- direction to select the output.
directionElim :: Direction -> a -> a -> a
directionElim Backward b _ = b
directionElim Forward  _ f = f

-- | A mark in a buffer
newtype Mark = Mark {markId :: Int} deriving (Eq, Ord, Show, Typeable, Binary)

-- | Reference to a buffer.
newtype BufferRef = BufferRef Int
    deriving (Eq, Ord, Typeable, Binary)

instance Show BufferRef where
    show (BufferRef r) = "B#" ++ show r

-- | A point in a buffer
newtype Point = Point {fromPoint :: Int}           -- offset in the buffer (#codepoints, NOT bytes)
    deriving (Eq, Ord, Enum, Bounded, Typeable, Binary, Ix)

instance Show Point where
    show (Point p) = show p

-- | Size of a buffer region
newtype Size = Size {fromSize :: Int}             -- size in bytes (#bytes, NOT codepoints)
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Binary)

instance SemiNum Point Size where
    Point p +~ Size s = Point (p + s)
    Point p -~ Size s = Point (p - s)
    Point p ~- Point q = Size (abs (p - q))

fromString :: String -> Rope
fromString = R.fromString

-- | Window references
newtype WindowRef = WindowRef { unWindowRef :: Int }
  deriving(Eq, Ord, Enum, Show, Typeable, Binary)

instance Initializable WindowRef where initial = WindowRef (-1)

-- | The region data type. 
--The region is semi open: it includes the start but not the end bound. This allows simpler region-manipulation algorithms.
-- Invariant : regionStart r <= regionEnd r
data Region = Region {regionDirection :: !Direction,
                      regionStart, regionEnd :: !Point} 
                 deriving (Typeable)

$(derive makeBinary ''Region)

instance Show Region where
    show r = show (regionStart r) ++ 
             (case regionDirection r of
               Forward -> " -> " 
               Backward -> " <- " 
             ) ++ 
             show (regionEnd r)

regionFirst :: Region -> Point
regionFirst (Region Forward p _) = p
regionFirst (Region Backward _ p) = p

regionLast :: Region -> Point
regionLast (Region Forward _ p) = p
regionLast (Region Backward p _) = p


fmapRegion :: (Point -> Point) -> Region -> Region
fmapRegion f (Region d x y) = Region d (f x) (f y)

regionSize :: Region -> Size
regionSize r = regionEnd r ~- regionStart r

-- | Take the intersection of two regions
intersectRegion :: Region -> Region -> Region
intersectRegion (Region _ x1 y1) (Region _ x2 y2) = ordRegion (max x1 x2) (min y1 y2)

-- | Take the union of two regions (including what is between them)
unionRegion :: Region -> Region -> Region
unionRegion (Region _ x1 y1) (Region _ x2 y2) = mkRegion (min x1 x2) (max y1 y2)


-- | Create a region from ordered bounds. If 2nd argument is greater than
-- 1st, then the region will be empty.
ordRegion :: Point -> Point -> Region
ordRegion x y = if x < y then Region Forward x y else emptyRegion

-- | Construct a region from its bounds, emacs style:
-- the right bound is excluded
mkRegion :: Point -> Point -> Region
mkRegion x y = if x <= y then Region Forward x y else Region Backward y x

mkRegion' :: Direction -> Point -> Point -> Region
mkRegion' d x y = if x <= y then Region d x y else Region d y x

mkSizeRegion :: Point -> Size -> Region
mkSizeRegion x s = mkRegion x (x +~ s)

-- | The empty region
emptyRegion :: Region
emptyRegion = Region Forward (Point 0) (Point 0)

-- | True if the given point is inside the given region.
inRegion :: Point -> Region -> Bool
p `inRegion` (Region _ start stop) = start <= p && p < stop

-- | True if the given point is inside the given region or at the end of it.
nearRegion :: Point -> Region -> Bool
p `nearRegion` (Region _ start stop) = start <= p && p <= stop

-- | Returns if a region (1st arg) is  included in another (2nd arg)
includedRegion :: Region -> Region -> Bool
r0 `includedRegion` r = regionStart r <= regionStart r0 && regionEnd r0 <= regionEnd r

regionIsEmpty :: Region -> Bool
regionIsEmpty (Region _ start stop) = start >= stop

regionsOverlap :: Bool -> Region -> Region -> Bool
regionsOverlap border (Region _ x1 y1) (Region _ x2 y2) =
    cmp x2 y1 y2 || cmp x2 x1 y2 ||
    cmp x1 y2 y1 || cmp x1 x2 y1
  where
    cmp a b c = a <= b && if border then b <=c  else b < c
