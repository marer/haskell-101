{-# LANGUAGE NoImplicitPrelude #-}
module Codewars.FiveFundamentalMonads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
    return = Identity
    (Identity v) >>= f = f v

instance Monad Maybe where
    return = Just
    Nothing >>= f = Nothing
    (Just v) >>= f = f v

instance Monad (State s) where
    return a = State { runState = \s -> (a, s) } 
    (State g) >>= f = undefined

    -- p >>= k = state $ \ s0 ->
    --     let (x, s1) = runState p s0  -- Running the first processor on s0.
    --     in runState (k x) s1         -- Running the second processor on s1.

instance Monad (Reader s) where
  return = undefined
  (Reader g) >>= f = undefined

instance Monoid w => Monad (Writer w) where
  return = undefined
  (Writer (s, v)) >>= f = undefined
