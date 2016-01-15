{-# LANGUAGE GADTs #-}
module Control.Monad.HistoryState where

import Control.Monad
import Control.Applicative


newtype HistoryState f s a = HistoryState {
        runHistoryState :: f s -> (f s, a)
   }

class HistoryStateMonad f i s where
      get :: i -> HistoryState s (f s)
      set :: s -> HistoryState s ()
      replace :: f s -> HistoryState s ()
      goback :: i -> HistoryState s ()
      prune :: i -> HistoryState s ()


instance Functor (HistoryState s) where
         fmap f m = HistoryState $ \s -> let (s',a) = runHistoryState m s
                                         in (s', f a)

instance Applicative (HistoryState s) where
         pure = return
         (<*>) = ap

instance Monad (HistoryState s) where
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
     pp <- get 10
     forM_ (pp `zip` [1..]) $ \(p,i) -> set (p * i)
     goback 5
     forM_ (pp `zip` [1..]) $ \(p,i) -> set (p + i)
