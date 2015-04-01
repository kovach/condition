-- TODO
-- write simple HM using Condition as example
-- Monad.Condition.Util?
--   - help with exploring output
module Monad.Condition
  ( Name, Condition, WrittenEnv
  , runCondition
  , get, store, put, filterEnv, modifyAll
  , printEnv, writeEnv, writeEnvF
  , amb, exit, assert, kill
  ) where

import Prelude hiding (not)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.Class
-- Use MTL for MonadState class
import Control.Monad.State hiding (get, put)
import qualified Control.Monad.State as S (get, put)
import Control.Monad.Trans.Either
import Data.Either (isLeft)
import Control.Arrow (first, second)

newtype Name = N Integer
 deriving (Eq, Ord)

toInt :: Name -> Integer
toInt (N i) = i

instance Show Name where
  show (N i) = "#" ++ show i


type Env a = (Integer, M.Map Name a)
type WrittenEnv a = (Integer, [(Integer, a)])

emptyChain :: Env a
emptyChain = (0, M.empty)

-- "\s -> [(Either e x, s)]".   where s = Env v
type Condition e v = EitherT e (StateT (Env v) [])

writeEnv :: Env a -> WrittenEnv a
writeEnv = second $ map (first toInt) . M.toList

writeEnvF :: Functor f => Env (f Name) -> WrittenEnv (f Integer)
writeEnvF = second $ map (\(N int, val) -> (int, fmap toInt val)) . M.toList

runCondition :: Condition e s a -> [(Either e a, Env s)]
runCondition m = runStateT (runEitherT m) emptyChain

get :: Name -> Condition e a a
get name = do
  (_, m) <- S.get
  return $ fromJust . M.lookup name $ m

store :: a -> Condition e a Name
store val = do
  (count, env) <- S.get
  let name = N (count + 1)
  S.put (count + 1, M.insert name val env)
  return name

put :: Name -> a -> Condition e a ()
put name val = modify $ \(c, env) -> (c, M.insert name val env)

-- Returns all objects that refer to given name
filterEnv :: (Name -> a -> Maybe b) -> Condition e a [b]
filterEnv fn = do
  (_, env) <- S.get
  return $ mapMaybe (uncurry fn) $ M.toList env

modifyAll :: (Name -> a -> a) -> Condition e a ()
modifyAll fn = do
  (c, env) <- S.get
  S.put (c, M.mapWithKey fn env)

printEnv :: Show a => Condition e a String
printEnv = do
  (_, env) <- S.get
  return (unlines $ map show (M.toList env))

-- Must explicitly define this; `mplus` defaults to EitherT
-- TODO replace ++ with interleave?
amb :: Condition e a b -> Condition e a b -> Condition e a b
amb m1 m2 = EitherT $ StateT $ \s -> 
  (runStateT (runEitherT m1) s) ++ (runStateT (runEitherT m2) s)

-- Logical negation
-- soft failure -> success
-- success -> hard failure
-- (hard failure remains hard failure)
-- TODO is this good?
not :: Condition e a b -> Condition e a ()
not m = EitherT $ StateT $ \s0 ->
  runStateT (runEitherT m) s0 $>
  filter (isLeft . fst) $>
  map (\(_, s) -> (Right (), s))
 where
   ($>) = flip ($)

-- TODO 'fail' is not left :| not sure how to fix
exit :: e -> Condition e a b
-- Soft failure
exit = left

kill :: Condition e a b
-- Hard failure, no record left
kill = EitherT (StateT (\s -> []))

assert :: Bool -> e -> Condition e a ()
assert cond msg =
  if cond then return () else exit msg

-- Test
p1 = do
  x <- store 0 `amb` store 1 `amb` store 2
  y <- store 2
  val <- get x
  not (assert (val == 0) $ show val)
  return (x, y)


