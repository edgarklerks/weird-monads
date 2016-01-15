{-# LANGUAGE GADTs #-}
module Control.Monad.StreamPipe where

import Control.Monad
import Control.Applicative

data Pipe i o a where
     Input :: (i -> Pipe i o a) -> Pipe i o a
     Yield :: (() -> (o, Pipe i o a)) -> Pipe i o a
     Pure :: a -> Pipe i o a

newtype InputPipe i e o = InputPipe {
                  runInputPipe :: Pipe i o e
          }

instance Functor (InputPipe i e) where
    fmap f m = InputPipe $ fmap' f (runInputPipe m)
        where fmap' f (Pure a) = Pure a
              fmap' q (Input f) = Input $ \i ->  runInputPipe $fmap q $ InputPipe (f i)
              fmap' f (Yield g) = Yield $ \i -> let (o, p) = g i
                                                in (f o, runInputPipe $ fmap f (InputPipe p))

instance Applicative (InputPipe i e) where
         pure a = InputPipe $ Yield (\() -> (a, runInputPipe $ pure a))
         (<*>) f g = InputPipe $ worker (runInputPipe f) (runInputPipe g)
                where worker (Input f) (Input g) = Input $ \i -> runInputPipe $ InputPipe ( f i) <*> InputPipe ( g i)
                      worker _ (Pure g) =  Pure g
                      worker (Pure a) _ = Pure a
                      worker (Yield f)  (Yield g) = Yield $ \i -> let (f', pf') = f i
                                                                      (a', pf'') = g i
                                                                  in (f' a', runInputPipe $ InputPipe pf' <*> InputPipe pf'')
                      worker (Input f) q = Input $ \i -> runInputPipe $ InputPipe (f i) <*> InputPipe q
                      worker q (Input g) =  Input $ \i -> runInputPipe $ InputPipe q <*>  InputPipe (g i)

instance Show (Pipe i o a) where
         show (Input _) = "Input state"
         show (Yield _) = "Yield state"
         show (Pure a) = "Pure state"

instance Functor (Pipe i o) where
         fmap f (Pure a) = Pure (f a)
         fmap f (Yield g) = Yield $ \() -> let (o, p) = g ()
                                                                in  (o, fmap f p)
         fmap f (Input g) = Input $ \i -> let (p) =  g i
                                           in (fmap f p)


instance Applicative (Pipe i o) where
         pure = return
         (<*>) = ap

instance Monad (Pipe i o) where
         return = Pure
         (>>=) (Pure a) f = f a
         (>>=) (Yield g) f = Yield $ \i -> let (o, p) = g i
                                                     in (o, p >>= f)
         (>>=) (Input g) f = Input $ \i -> let (p) = g i
                                                      in (p >>= f)


stepPipe :: Pipe i o a -> i -> Either a (Maybe o, Pipe i o a)
stepPipe (Yield g) _ = Right $ (let (o,p) = g () in (Just o, p)
                                          )
stepPipe (Input f) i = Right $ (Nothing,  f i)
stepPipe (Pure a) _ = Left $ a

data PipeState i o a = Partial (Pipe i o a)
                     | Evaluated ([i], a)
                     | Output ([i], o, Pipe i o a)
     deriving (Show)


-- stepTillOutput :: Pipe i o a -> [i] -> ([i], Either a (o, Pipe i o a))
stepTillOutput (Pure a) xs = Evaluated (xs, a)
stepTillOutput (Yield g) xs = let (o, p) = g ()
                              in  Output (xs, o, p)
stepTillOutput t@(Input f) [] = Partial t
stepTillOutput (Input f) (x:xs) = let p = f x
                                  in stepTillOutput p xs

runPipe'' :: Pipe i o a -> [i] -> ([o], Maybe a)
runPipe'' (Pure a)  _ = ([], Just a)
runPipe'' (Yield g)  (xs) = let (o, p) = g ()
                                (rs, a) = runPipe'' p xs
                            in (o:rs, a)
runPipe'' (Input _) [] = ([], Nothing)
runPipe'' (Input f)  (x:xs) = let p = f x
                              in runPipe'' p xs




runPipe' :: Pipe i o a -> Int -> [i] -> ([o], Maybe a)
runPipe' (Pure a) n xs = ([], Just a)
runPipe' (Yield g) n (xs) = let (o, p) = g ()
                                (rs, a) = runPipe' p (n - 1) xs
                            in (o:rs, a)
runPipe' (Input f) n (x:xs) = let p = f x
                              in runPipe' p (n - 1) xs


runPipe :: Pipe i o a -> [i] -> ([o], Maybe a)
runPipe p [] = ([], Nothing)
runPipe p (i:is) = let res = stepPipe p i
                   in case res of
                        Left a -> ([],Just a)
                        Right (Just o, p') -> let (xs, a) = runPipe p' is
                                              in (o:xs, a)
                        Right (Nothing, p') -> let (xs, a) = runPipe p' is
                                               in (xs, a)

input :: Pipe i o i
input = Input $ \i -> (Pure i)

yield :: o -> Pipe i o ()
yield o = Yield $ \i -> (o, Pure ())

-- feed :: Pipe i j e -> Pipe j k e -> Pipe i k e
feed p q = do
     input >>= p >>= yield >>= q

-- | Find tuples of odd naturals with the property:
-- x^2 % y == x
pairFinder xs = fst $ runPipe'' pairFinder' $ join [[x,y]| x <- xs, y <- xs]
           where pairFinder' = forever $ do
                    x <- input
                    y <- input
                    when ( ((x * x) `mod` y == x)) $ yield (x,y)

gen = forever $ yield 1

newtype HistoryState s a = HistoryState {
        runHistoryState :: [s] -> ([s], a)
   }

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


get :: Int -> HistoryState s [s]
get n = HistoryState $ \s -> (s, take n s)

set :: s -> HistoryState s ()
set x = HistoryState $ \s -> (x:s, ())

prune :: Int -> HistoryState s ()
prune n = HistoryState $ \s -> (reverse . drop n . reverse $ s , ())

goback :: Int -> HistoryState s ()
goback n = HistoryState $ \s -> (drop n s, ())

test = runHistoryState $ do
     pp <- get 10
     forM_ (pp `zip` [1..]) $ \(p,i) -> set (p * i)
     goback 5
     forM_ (pp `zip` [1..]) $ \(p,i) -> set (p + i)
