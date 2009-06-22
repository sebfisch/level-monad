-- |
-- Module      : Control.Monad.Levels
-- Copyright   : Sebastian Fischer
-- License     : PublicDomain
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This library provides an implementation of the MonadPlus type class
-- that enumerates the levels of the search space and allows to
-- implement breadth-first search and iterative deepening depth-first
-- search.
-- 
-- The implementation is inspired by Mike Spivey and Silvija Seres:
-- see Chapter 9 of the book 'The Fun of Programming' and the paper
-- 'Algebras for Combinatorial Search'.
-- 
-- The implementation of breadth-first search is similar to the
-- inspiring implementation but uses lists with constant-time
-- concatenation to represent levels. The implementation of iterative
-- deepening depth-first is simpler than the inspiring implementation
-- thanks to the use of a continuation monad.
module Control.Monad.Levels ( bfs, idfs, idfsBy ) where

import Data.Monoid
import Data.FMList


-- | The function @bfs@ enumerates the results of a non-deterministic
-- computation using breadth-first search. The implementation does not
-- guarantee that results are returned in any specific order but it
-- does guarantee that every result is eventually enumerated. Due to
-- the large memory requirements of breadth-first search you should
-- use @idfs@ for expensive search.
bfs :: FMList a -> [a]
bfs a = runLevels (unFM a yield)
 where yield x = Levels [singleton x]

-- Non-Deterministic computations of type @Levels a@ can be searched
-- level-wise.
newtype Levels a = Levels { levels :: [FMList a] }

-- Concatenates levels amd yields result as list. 
runLevels :: Levels a -> [a]
runLevels = toList . foldr append empty . levels

instance Monoid (Levels a) where
  mempty        = Levels []
  a `mappend` b = Levels (empty : merge (levels a) (levels b))

-- like 'zipWith append' without cutting the longer list
merge :: [FMList a] -> [FMList a] -> [FMList a]
merge []      ys    = ys
merge xs      []    = xs
merge (x:xs) (y:ys) = append x y : merge xs ys

-- | The function @idfs@ computes the levels of a depth bound
-- computation using iterative deepening depth-first search. Unlike
-- breadth-first search it runs in constant space but usually takes a
-- bit longer, depending on how the depth limit is increased. Use
-- @idfsBy@ to control this. Don't use this algorithm if you know that
-- there is only a finite number of results because it will continue
-- trying larger depth limits without recognizing that there are no
-- more solutions.  It can, however, produce results lazily: calling
-- @take n . idfs@ terminates if the number of results is larger than
-- @n@.
idfs :: FMList a -> [a]
idfs = idfsBy 100

-- | The function @idfsBy@ computes the levels of a depth bound
-- computation using iterative deepening depth-first search
-- incrementing the depth limit between searches using the given
-- number of steps.
idfsBy :: Int -> FMList a -> [a]
idfsBy n a = toList $ foldr append empty [ unFM a yield ! d | d <- [0,n..] ]
 where yield x = DepthBound (\d -> if d<n then singleton x else empty)

-- The type @DepthBound@ represents computations with a bounded depth
-- to iterative deepening search.
newtype DepthBound a = DepthBound { (!) :: Int -> FMList a }

instance Monoid (DepthBound a) where
  mempty        = DepthBound (const empty)
  a `mappend` b = DepthBound (\d -> if d==0 then empty
                                    else append (a!(d-1)) (b!(d-1)))
