{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.HistoryState where

import Control.Monad
import Control.Applicative
import qualified Text.PrettyPrint.ANSI.Leijen as L
import Data.Maybe

-- | Needed for representing history as a tree like phenomena
data RoseTree l a where
       Node :: [(RoseTree l a, l,  a)] -> RoseTree l a
       Empty :: RoseTree l a
     deriving Show

extendDown :: Enum l => (a) -> RoseTree l a -> RoseTree l a
extendDown (a) (Empty) = Node [(Empty, toEnum 0,a)]
extendDown (a) (Node []) = Node [(Empty, toEnum 0, a)]
extendDown (a) (Node (x@(_,l,_) :xs)) = Node ((Node [x],succ l,a) : xs)

dropDown :: Integer -> RoseTree l a -> RoseTree l a
dropDown 0 x = x
dropDown n Empty = Empty
dropDown n (Node ((r,l,a) :xs)) = dropDown (pred n) r

dupLeft :: (Enum l,  Monoid a) => RoseTree l a -> RoseTree l a
dupLeft (Empty) = Node [(Empty, toEnum 0, mempty)]
dupLeft (Node []) = Node [(Empty, toEnum 0, mempty)]
dupLeft (Node ((r,l,a) :xs)) = Node ((r,succ l, a): (r,l,a) :xs)

prunePar :: Integer -> RoseTree l a -> RoseTree l a
prunePar 0 x = Empty
prunePar n Empty = Empty
prunePar n (Node xs) = Node $ (\(r,l,a) -> (prunePar (pred n) r, l , a)) <$> xs

modFirst :: Enum l => RoseTree l a -> (Maybe a -> a ) -> RoseTree l a
modFirst (Empty) f = Node [(Empty, toEnum 0, f Nothing)]
modFirst (Node ((l,r,a):xs)) f = Node ((l,r,f $ Just a):xs)

headValue :: RoseTree l a -> Maybe a
headValue Empty = Nothing
headValue (Node ((r,l,a) :xs)) = Just a


headLabel :: RoseTree l a -> Maybe l
headLabel Empty = Nothing
headLabel (Node ((r,l,a):xs)) = Just l

-- | takes the left most branch
headBranch :: RoseTree l a -> [(l, a)]
headBranch (Empty) = []
headBranch (Node ((r,l,a):xs)) = (l, a) : headBranch r


-- | shows a history tree where vertical signifies different events and horizontal signifies the time
ppRoseTree :: (L.Pretty l, L.Pretty a) => RoseTree l a -> L.Doc
ppRoseTree (Empty) = L.red $ L.text "[]"
ppRoseTree (Node []) = L.green (L.text "Node") L.</> L.magenta (L.text "[]")
ppRoseTree (Node xs) = keyword "Node" L.<//> btsep (worker xs)
           where sepl = L.magenta $ L.char '['
                 sepr = L.magenta $ L.char ']'
                 enl = L.green $ L.char '('
                 enr = L.green $ L.char ')'
                 bten = L.enclose enl enr
                 btsep = L.enclose sepl sepr . L.align
                 keyword s = L.cyan $ L.pretty s
                 value l = L.red $ L.pretty l
                 label l = L.blue $ L.pretty l
                 worker ((r,l, a):xs) =
                                       bten (
                                                 label l
                                          L.</> L.char '|'
                                          L.</> value a
                                      )
                                 L.</> L.yellow (L.text "->") L.</>
                                id ( btsep (
                                                                      L.line
                                                                 L.<> ( ppRoseTree r )
                                                                 L.<> L.comma
                                                                 L.<$$> worker xs
                                                                )
                                                  )

                 worker []  = mempty

-- | A monad, which is a normal state monad:
--   State monad with the newtype constructor removed:
--     type State s a = s -> (s, a)
--   We can rewrite the history state monad in terms of the state monad:
--     type HistoryState' f s a = State (f s) a
--   Expand the State monad:
--     type HistoryState' f s a = f s -> (f s, a)
--   Which is the same as definition below modulo newtype
-- Thus it is just a reinterpretation of the normal state monad.
newtype HistoryState f s a = HistoryState {
        runHistoryState :: f s -> (f s, a)
   }


newtype IntegerPlus = IntegerPlus {
                    unIntegerPlus :: Integer
        } deriving (Num, Integral, Enum, Real, Ord, Eq, Show, L.Pretty)

instance Monoid IntegerPlus where
         mempty = IntegerPlus 0
         mappend a b = a * b


testx :: HistoryState (RoseTree Integer) IntegerPlus IntegerPlus
testx = do
        q <- focus
        case mod <$> q <*> ( pure 31 ) of
          Nothing -> return mempty
          Just 0 -> branch (\q -> return $ maybe 23 (+23) q)
          Just n -> branch (\q -> return $ maybe (n) (+n) q)





-- | We can view the history as a tree of states, where the branches of the state expand horizontal and time moves vertical.
class TreeHistoryStateMonad f i s | f -> i where
      right :: i -> HistoryState f s ()
      down :: i -> HistoryState f s ()
      --
      focus :: HistoryState f s (Maybe s)
            -- [branch m][f s] = [ let q = ex fs]
      --                   [m [Just q]] <-> [q] <-> [f s]
      -- [branch m][0] = [m Nothing] <-> [f s]
      --
      branch :: (Maybe s -> HistoryState f s a) -> HistoryState f s a




instance (Enum l, Monoid s) => TreeHistoryStateMonad (RoseTree l) Integer s where
      right n  = HistoryState $ \s -> (goLeft s n, ())
                           where goLeft (Node xs) n = Node (switch n xs)
                                 goLeft Empty _ = Empty
      down n = HistoryState $ \s -> (dropDown n s, ())
      focus = HistoryState $ \s -> (s,headValue s)
      branch f = HistoryState $ \s ->  let ss = ((dupLeft s))
                                           m = f $ headValue ss
                                           (h, a) = runHistoryState m ss
                                       in (h, a)




switch ::  Integer -> [a] -> [a]
switch s xs = case getMaybe s xs of
                  Nothing -> xs
                  Just (x,xs) -> (x:xs)

getMaybe :: Integer -> [a] -> Maybe (a, [a])
getMaybe _ [] = Nothing
getMaybe n xs = worker n xs []
        where worker _ [] _ = Nothing
              worker 0 (x:xs) z = Just (x,reverse z ++ xs)
              worker n (x:xs) z = worker (n - 1) xs (x : z)


class HistoryStateMonad f i s | f -> i where
      set :: s -> HistoryState f s ()
      replace :: f s -> HistoryState f s ()



class LinearHistoryStateMonad f i s | f -> i where
        get :: i -> HistoryState f s [s]
        goback :: i -> HistoryState f s ()
        prune :: i -> HistoryState f s ()

instance (Enum l, Monoid s) => LinearHistoryStateMonad (RoseTree l) Integer s where
         goback = down
         -- | prunes every possible history from n
         prune n = HistoryState $ \s -> (prunePar n s, ())
         -- | Find a history with a time length of at most n  and return everything what happened from there
         get n = HistoryState $ \s -> (s, findBranchWithMax n s)

findBranchWithMax :: Integer -> RoseTree l a -> [a]
findBranchWithMax n (Empty) = []
findBranchWithMax 0 _ = []
findBranchWithMax n (Node (xs)) =  longest 0 []  $ (\(r, l, x) ->  x : findBranchWithMax (n - 1) r) <$> xs
                  where longest s c (x:xs) | ls > s = longest ls x xs
                                           | otherwise = longest s c xs
                                    where ls = length x
                        longest s c [] = c


instance (Enum l,  Monoid s) => HistoryStateMonad (RoseTree l) Integer s where
         set v = HistoryState $ \s -> (extendDown v s, ())
         replace fs = HistoryState $ \_ -> (fs, ())

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
        set x = HistoryState $ \s -> (x:s, ())
        replace fs = HistoryState $ \_ -> (fs, ())

instance LinearHistoryStateMonad [] Int s where
        get n = HistoryState $ \s -> (s, take n s)
        prune n = HistoryState $ \s -> (reverse . drop n . reverse $ s , ())
        goback n = HistoryState $ \s -> (drop n s, ())

test :: RoseTree Integer IntegerPlus -> (RoseTree Integer IntegerPlus, ())
test = runHistoryState $ do
     pp <- get 10000
     set (head (pp ) + ( 1 :: IntegerPlus))
     forM_ ((pp) `zip` [1..]) $ \((p),i) -> set (p * i)
     goback 5
     forM_ ((pp) `zip` [1..]) $ \((p),i) -> branch (\c -> case c of
                                                                              Nothing -> set (p + i)
                                                                              Just n -> set (p + n)
                                                                  )
