{-# LANGUAGE GADTs #-}
module Control.Monad.StreamPipe where

import Control.Monad
import Control.Applicative

data Pipe i o a where
     Input :: (i -> Pipe i o a) -> Pipe i o a
     Yield :: (() -> (o, Pipe i o a)) -> Pipe i o a
     Pure :: a -> Pipe i o a

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

-- | Done one step of the computation
stepPipe :: Pipe i o a -> i -> Either a (Maybe o, Pipe i o a)
stepPipe (Yield g) _ = Right $ (let (o,p) = g () in (Just o, p)
                                          )
stepPipe (Input f) i = Right $ (Nothing,  f i)
stepPipe (Pure a) _ = Left $ a

data PipeState i o a = Partial (Pipe i o a)
                     | Evaluated ([i], a)
                     | Output ([i], o, Pipe i o a)
     deriving (Show)

-- | Step till the first output
stepTillOutput :: Pipe i o a -> [i] -> PipeState i o a
stepTillOutput (Pure a) xs = Evaluated (xs, a)
stepTillOutput (Yield g) xs = let (o, p) = g ()
                              in  Output (xs, o, p)
stepTillOutput t@(Input f) [] = Partial t
stepTillOutput (Input f) (x:xs) = let p = f x
                                  in stepTillOutput p xs

-- | Steps through the pipe till the input is empty or the pipe ends
runPipe'' :: Pipe i o a -> [i] -> ([o], Maybe a)
runPipe'' (Pure a)  _ = ([], Just a)
runPipe'' (Yield g)  (xs) = let (o, p) = g ()
                                (rs, a) = runPipe'' p xs
                            in (o:rs, a)
runPipe'' (Input _) [] = ([], Nothing)
runPipe'' (Input f)  (x:xs) = let p = f x
                              in runPipe'' p xs




-- | `runPipe pipe n is`
-- Run a pipe while n > 0, assumes length is >= n
runPipe' :: Pipe i o a -> Int -> [i] -> ([o], Maybe a)
rynPipe' _ 0 _ = ([], Nothing)
runPipe' (Pure a) _ _ = ([], Just a)
runPipe' (Yield g) n (xs) = let (o, p) = g ()
                                (rs, a) = runPipe' p (n - 1) xs
                            in (o:rs, a)
runPipe' (Input f) n (x:xs) = let p = f x
                              in runPipe' p (n - 1) xs

-- | Run a pipe while continously consuming input, but not always use it in the computation.

runPipe :: Pipe i o a -> [i] -> ([o], Maybe a)
runPipe p [] = ([], Nothing)
runPipe p (i:is) = let res = stepPipe p i
                   in case res of
                        Left a -> ([],Just a)
                        Right (Just o, p') -> let (xs, a) = runPipe p' is
                                              in (o:xs, a)
                        Right (Nothing, p') -> let (xs, a) = runPipe p' is
                                               in (xs, a)

-- | Read from the outer context
input :: Pipe i o i
input = Input $ \i -> (Pure i)

-- | Give control back to the outer context. Sends an argument with it.
yield :: o -> Pipe i o ()
yield o = Yield $ \i -> (o, Pure ())

-- | Find tuples of odd naturals with the property:
-- x^2 % y == x
pairFinder :: [Integer] -> [(Integer, Integer)]
pairFinder xs = fst $ runPipe'' pairFinder' $ join [[x,y]| x <- xs, y <- xs]
           where pairFinder' = forever $ do
                    x <- input
                    y <- input
                    when ( ((x * x) `mod` y == x)) $ yield (x,y)

-- | Hello world of generators
gen1 :: Pipe t Int s
gen1 = forever $ yield 1

-- | InputPipe is a reinterpretation of Pipe and doesn't have a monad instance.
-- But it has an interesting applicative instance.
-- It is an infinite stream, which can throw an exception.
newtype InputPipe e o = InputPipe {
                  runInputPipe :: Pipe () o e
          }

instance Functor (InputPipe e) where
    fmap f m = InputPipe $ fmap' f (runInputPipe m)
        where fmap' f (Pure a) = Pure a
              fmap' q (Input f) = Input $ \i ->  runInputPipe $fmap q $ InputPipe (f i)
              fmap' f (Yield g) = Yield $ \i -> let (o, p) = g i
                                                in (f o, runInputPipe $ fmap f (InputPipe p))

instance Applicative (InputPipe e) where
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
