-- |
-- Module      : Control.Monad.Levels
-- Copyright   : Sebastian Fischer
-- License     : PublicDomain
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This library provides an implementation of the MonadPlus type
-- class that enumerates the levels of the search space and allows to
-- implement breadth-first search.
-- 
-- The implementation is inspired by Mike Spivey and Silvija Seres:
-- cf. Chapter 9 of the book 'The Fun of Programming'.
-- 
-- Warning: @Levels@ is only a monad when the results of the
-- enumeration functions are interpreted as a multiset, i.e., a valid
-- transformation according to the monad laws may change the order of
-- the results.

module Control.Monad.Levels ( 

  Levels, levels, breadthFirstSearch, 

  DepthBound, iterLevels, iterativeDeepening,

  diagonals

  ) where

import Control.Monad

-- | 
-- Non-Deterministic computations of type @Levels a@ can be searched
-- level-wise.
newtype Levels a = Levels { 

  -- |
  -- The function @levels@ yields the results of a non-deterministic
  -- computation grouped in levels.
  levels :: [[a]]

  }

-- |
-- The function @breadthFirstSearch@ enumerates the results of a
-- non-deterministic computation in breadth-first order.
breadthFirstSearch :: Levels a -> [a]
breadthFirstSearch = concat . levels

instance Monad Levels
 where
  return x = Levels [[x]]

  Levels x >>= f = Levels (x `bind` (levels . f))

  fail _ = Levels []

bind :: [[a]] -> (a -> [[b]]) -> [[b]]
bind x f = map concat (diagonals (map (foldr zipConc [] . map f) x))

instance MonadPlus Levels
 where
  mzero = Levels []

  Levels xs `mplus` Levels ys = Levels ([] : zipConc xs ys)

-- |
-- The type @DepthBound@ represents computations with a bounded
-- depth. It's monad instances implements iterative deepening.
newtype DepthBound a = DepthBound { (!) :: Int -> [(a,Int)] }

instance Monad DepthBound
 where
  return x = DepthBound (\d -> [(x,d)])
  a >>= f  = DepthBound (\d -> [ y | (x,d') <- a!d, y <- f x!d' ])
  fail _   = DepthBound (const [])

instance MonadPlus DepthBound
 where
  mzero       = DepthBound (const [])
  a `mplus` b = DepthBound (\d -> do guard (d>0)
                                     let d' = d-1
                                     (a!d') `mplus` (b!d'))

-- |
-- The function @iterLevels@ computes the levels of a depth bound
-- computation using iterative deepening.
iterLevels :: DepthBound a -> Levels a
iterLevels a = Levels [[ x | (x,0) <- a!d ] | d <- [0..]]

-- |
-- The function @iterativeDeepening@ enumerates the results of a
-- non-deterministic computations using iterative deepening.
iterativeDeepening :: DepthBound a -> [a]
iterativeDeepening = concat . levels . iterLevels

-- | 
-- The function @diagonals@ enumarates the entries of a matrix
-- diagonally. The matrix may contain an infinite number of infinite
-- rows.
diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals (xs:xss) = zipConc [[x] | x <- xs] ([] : diagonals xss)

zipConc :: [[a]] -> [[a]] -> [[a]]
zipConc []       yss      = yss
zipConc xss      []       = xss
zipConc (xs:xss) (ys:yss) = (xs++ys) : zipConc xss yss
