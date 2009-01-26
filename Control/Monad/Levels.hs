-- |
-- Module      : Control.Monad.Levels
-- Copyright   : Sebastian Fischer 2009
-- License     : BSD3
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
-- Warning: @Levels@ is only a monad when the results of
-- @breadthFirstSearch@ or @levelDiagonalisation@ are interpreted as a
-- set; that is, a valid transformation according to the monad laws
-- may change the order of the results.

module Control.Monad.Levels ( 

  Levels, levels, breadthFirstSearch --, levelDiagonalisation

  ) where

import Control.Monad

-- | 
-- Non-Deterministic computations of type @Levels a@ can be searched
-- level-wise.
newtype Levels a = Levels { levels :: [[a]] }

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
x `bind` f = map concat (diagonals (map (foldr zipConc [] . map f) x))

diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals (xs:xss) = zipConc [[x] | x <- xs] ([] : diagonals xss)

zipConc :: [[a]] -> [[a]] -> [[a]]
zipConc []     ys     = ys
zipConc xs     []     = xs
zipConc (x:xs) (y:ys) = (x++y) : zipConc xs ys

instance MonadPlus Levels
 where
  mzero = Levels []

  Levels xs `mplus` Levels ys = Levels ([] : zipConc xs ys)

