{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      :  Yi.Interact
License     :  GPL-2
Maintainer  :  yi-devel@googlegroups.com
Stability   :  experimental
Portability :  portable

This is a library of interactive processes combinators, usable to
define extensible keymaps.

(Inspired by the Parsec library, written by Koen Claessen)

The processes are:

* composable: in parallel using '<|>', in sequence using monadic bind.

* extensible: it is always possible to override a behaviour by combination of
  'adjustPriority' and '<|>'. (See also '<||' for a convenient combination of the two.)

* monadic: sequencing is done via monadic bind. (leveraging the whole
  battery of monadic tools that Haskell provides)

The processes can parse input, and write output that depends on it.

The semantics are quite obvious; only disjunction
deserve a bit more explanation:

in @p = (a '<|>' b)@, what happens if @a@ and @b@ recognize the same
input (prefix), but produce conflicting output?

* if the output is the same (as by the Eq class), then the processes (prefixes) are "merged"
* if a Write is more prioritized than the other, the one with low priority will be discarded
* otherwise, the output will be delayed until one of the branches can be discarded.
* if there is no way to disambiguate, then no output will be generated anymore.
  This situation can be detected by using 'possibleActions' however.
-}

module Yi.Interact
    (
     I, P (Chain,End),
     InteractState (..),
     MonadInteract (..),
     deprioritize,
     important,
     (<||),
     (||>),
     option,
     oneOf,
     processOneEvent,
     computeState,
     event,
     events,
     mkAutomaton, idAutomaton,
     runWrite,
     anyEvent,
    ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Lens
import           Control.Monad.State hiding ( get, mapM )
import           Data.Foldable (asum)
import           Data.Function (on)
import           Data.List (groupBy)
import           Data.Monoid

------------------------------------------------
-- Classes

-- | Abstraction of monadic interactive processes
class (Eq w, Monad m, Alternative m, Applicative m, MonadPlus m) => MonadInteract m w e | m -> w e where
    write :: w -> m ()
    -- ^ Outputs a result.
    eventBounds :: Ord e => Maybe e -> Maybe e -> m e
    -- ^ Consumes and returns the next character.
    --   Fails if there is no input left, or outside the given bounds.
    adjustPriority :: Int -> m ()


-------------------------------------------------
-- State transformation

-- Needs -fallow-undecidable-instances
-- TODO: abstract over MonadTransformer
instance MonadInteract m w e => MonadInteract (StateT s m) w e where
    write = lift . write
    eventBounds l h = lift (eventBounds l h)
    adjustPriority p = lift (adjustPriority p)

---------------------------------------------------------------------------
-- | Interactive process description

-- TODO: Replace 'Doc:' by ^ when haddock supports GADTs
data I ev w a where
    Returns :: a -> I ev w a
    Binds :: I ev w a -> (a -> I ev w b) -> I ev w b
    Gets :: Ord ev => Maybe ev -> Maybe ev -> I ev w ev
    -- Doc: Accept any character between given bounds. Bound is ignored if 'Nothing'.
    Fails :: I ev w a
    Writes :: w -> I ev w ()
    Priority :: Int -> I ev w ()
    Plus :: I ev w a -> I ev w a -> I ev w a

instance Functor (I event w) where
  fmap f i = pure f <*> i

instance Applicative (I ev w) where
    pure = return
    a <*> b = do f <- a; x <- b; return (f x)

instance Alternative (I ev w) where
    empty = Fails
    (<|>) = Plus

instance Monad (I event w) where
  return  = Returns
  fail _  = Fails
  (>>=)   = Binds

instance Eq w => MonadPlus (I event w) where
  mzero = Fails
  mplus = Plus

instance Eq w => MonadInteract (I event w) w event where
    write = Writes
    eventBounds = Gets
    adjustPriority = Priority


infixl 3 <||

deprioritize :: (MonadInteract f w e) => f ()
deprioritize = adjustPriority 1

(<||), (||>) :: (MonadInteract f w e) => f a -> f a -> f a
a <|| b = a <|> (deprioritize >> b)

(||>) = flip (<||)

-- | Just like '(<||)' but in prefix form. It 'deprioritize's the
-- second argument.
important :: MonadInteract f w e => f a -> f a -> f a
important a b = a <|| b

-- | Convert a process description to an "executable" process.
mkProcess :: Eq w => I ev w a -> (a -> P ev w) -> P ev w
mkProcess (Returns x) = \fut -> fut x
mkProcess Fails = const Fail
mkProcess (m `Binds` f) = \fut -> mkProcess m (\a -> mkProcess (f a) fut)
mkProcess (Gets l h) = Get l h
mkProcess (Writes w) = \fut -> Write w (fut ())
mkProcess (Priority p) = \fut -> Prior p (fut ())
mkProcess (Plus a b) = \fut -> Best (mkProcess a fut) (mkProcess b fut)


----------------------------------------------------------------------
-- Process type

-- | Operational representation of a process
data P event w
    = Ord event => Get (Maybe event) (Maybe event) (event -> P event w)
    | Fail
    | Write w (P event w)
    | Prior Int (P event w) -- low numbers indicate high priority
    | Best (P event w) (P event w)
    | End
    | forall mid. (Show mid, Eq mid) => Chain (P event mid) (P mid w)

-- ---------------------------------------------------------------------------
-- Operations over P

runWrite :: Eq w => P event w -> [event] -> [w]
runWrite _ [] = []
runWrite p (c:cs) = let (ws, p') = processOneEvent p c in ws ++ runWrite p' cs

processOneEvent :: Eq w => P event w -> event -> ([w], P event w)
processOneEvent p e = pullWrites $ pushEvent p e

-- | Push an event in the automaton
pushEvent :: P ev w -> ev -> P ev w
pushEvent (Best c d) e = Best (pushEvent c e) (pushEvent d e)
pushEvent (Write w c) e = Write w (pushEvent c e)
pushEvent (Prior p c) e = Prior p (pushEvent c e)
pushEvent (Get l h f) e = if test (e >=) l && test (e <=) h then f e else Fail
    where test = maybe True
pushEvent Fail _ = Fail
pushEvent End _ = End
pushEvent (Chain p q) e = Chain (pushEvent p e) q

-- | Abstraction of the automaton state.
data InteractState event w =  Ambiguous [(Int,w,P event w)] | Waiting | Dead | Running w (P event w)

instance Monoid (InteractState event w) where
    -- not used at the moment:
    mappend (Running w c) _ = Running w c
    mappend _ (Running w c) = Running w c
    -- don't die if that can be avoided
    mappend Dead p = p
    mappend p Dead = p
    -- If a branch is not determined, wait for it.
    mappend Waiting _ = Waiting
    mappend _ Waiting = Waiting
    -- ambiguity remains
    mappend (Ambiguous a) (Ambiguous b) = Ambiguous (a ++ b)
    mempty = Ambiguous []


-- | find all the writes that are accessible.
findWrites :: Int -> P event w -> InteractState event w
findWrites p (Best c d) = findWrites p c `mappend` findWrites p d
findWrites p (Write w c) = Ambiguous [(p,w,c)]
findWrites p (Prior dp c) = findWrites (p+dp) c
findWrites _ Fail = Dead
findWrites _ End = Dead
findWrites _ (Get{})     = Waiting
findWrites p (Chain a b) = case computeState a of
    Dead -> Dead
    Ambiguous _ -> Dead -- If ambiguity, don't try to do anything clever for now; die.
    Running w c -> findWrites p (Chain c (pushEvent b w)) -- pull as much as possible from the left automaton
    Waiting -> case findWrites p b of
        Ambiguous choices -> Ambiguous [(p',w',Chain a c') | (p',w',c') <- choices]
        Running w' c' -> Running w' (Chain a c') -- when it has nothing more, pull from the right.
        Dead -> Dead
        Waiting -> Waiting

computeState :: Eq w => P event w -> InteractState event  w
computeState a = case findWrites 0 a of
    Ambiguous actions ->
      let prior = minimum $ map (view _1) actions
          bests = groupBy ((==) `on` view _2) $
                    filter ((prior ==) . view _1) actions
      in case bests of
        [(_,w,c):_] -> Running w c
        _ -> Ambiguous $ map head bests
    s -> s

pullWrites :: Eq w => P event w -> ([w], P event w)
pullWrites a = case computeState a of
    Running w c -> first (w:) (pullWrites c)
    _ -> ([], a)

instance (Show w, Show ev) => Show (P ev w) where
    show (Get Nothing Nothing _) = "?"
    show (Get (Just l) (Just h) _p) | l == h = show l -- ++ " " ++ show (p l)
    show (Get l h _) = maybe "" show l ++ ".." ++ maybe "" show h
    show (Prior p c) = ":" ++ show p ++ show c
    show (Write w c) = "!" ++ show w ++ "->" ++ show c
    show (End) = "."
    show (Fail) = "*"
    show (Best p q) = "{" ++ show p ++ "|" ++ show q ++ "}"
    show (Chain a b) = show a ++ ">>>" ++ show b

-- ---------------------------------------------------------------------------
-- Derived operations
oneOf :: (Ord event, MonadInteract m w event) => [event] -> m event
oneOf s = asum $ map event s

anyEvent :: (Ord event, MonadInteract m w event) => m event
anyEvent = eventBounds Nothing Nothing

event :: (Ord event, MonadInteract m w event) => event -> m event
-- ^ Parses and returns the specified character.
event e = eventBounds (Just e) (Just e)

events :: (Ord event, MonadInteract m w event) => [event] -> m [event]
-- ^ Parses and returns the specified list of events (lazily).
events = mapM event

option :: (MonadInteract m w e) => a -> m a -> m a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p `mplus` return x

mkAutomaton :: Eq w => I ev w a -> P ev w
mkAutomaton i = mkProcess i (const End)

-- An automaton that produces its input
idAutomaton :: (Ord a, Eq a) => P a a
idAutomaton = Get Nothing Nothing $ \e -> Write e idAutomaton
-- It would be much nicer to write:
--    mkAutomaton (forever 0 (anyEvent >>= write))
-- however this creates a memory leak. Unfortunately I don't understand why.
-- To witness:
--    dist/build/yi/yi +RTS -hyI -hd
-- Then type some characters. (Binds grows linearly)
