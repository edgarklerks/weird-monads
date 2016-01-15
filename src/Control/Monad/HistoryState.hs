{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.HistoryState where

import Control.Monad
import Control.Applicative


newtype HistoryState f s a = HistoryState {
        runHistoryState :: f s -> (f s, a)
   }

class HistoryStateMonad f i s | f -> i where
      get :: i -> HistoryState f s (f s)
      set :: s -> HistoryState f s ()
      replace :: f s -> HistoryState f s ()
      goback :: i -> HistoryState f s ()
      prune :: i -> HistoryState f s ()


instance Functor (HistoryState f s) where
         fmap f m = HistoryState $ \s -> let (s',a) = runHistoryState m s
                                         in (s', f a)

instance Applicative (HistoryState f s) where
         pure = return
         (<*>) = ap

instance Monad (HistoryState f s) where
         return a = HistoryState $ \s -> (s, a)
         (>>=) m f = HistoryState $ \s -> let (s', a) = runHistoryState m s
                                              (s'', b) = runHistoryState (f a) s'
                                    in (s'', b)


instance HistoryStateMonad [] Int s where
        get n = HistoryState $ \s -> (s, take n s)
        set x = HistoryState $ \s -> (x:s, ())
        prune n = HistoryState $ \s -> (reverse . drop n . reverse $ s , ())
        goback n = HistoryState $ \s -> (drop n s, ())
        replace fs = HistoryState $ \_ -> (fs, ())

test = runHistoryState $ do
     pp <- get 1
     set (head pp + 1)
     -- forM_ (pp `zip` [1..]) $ \(p,i) -> set (p * i)
     -- goback 5
     -- forM_ (pp `zip` [1..]) $ \(p,i) -> set (p + i)
