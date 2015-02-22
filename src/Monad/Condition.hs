-- TODO
-- write simple HM using Condition
--   Monad.Condition.Util?
--     - error printing
module Monad.Condition
  ( Name, Condition, runCondition
  , get, store, put, filterEnv, modifyAll
  , printEnv
  , amb, exit
  ) where

import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map as M
--import Control.Monad.Trans.State hiding (get, put)
--import qualified Control.Monad.Trans.State as S (get, put)
import Control.Monad
import Control.Monad.Trans.Class
-- Use MTL for MonadState class
import Control.Monad.State hiding (get, put)
import qualified Control.Monad.State as S (get, put)
import Control.Monad.Trans.Either
import Data.Either (isRight, isLeft)

newtype Name = N Integer
 deriving (Eq, Ord)

instance Show Name where
  show (N i) = "#" ++ show i

type CState a = (Integer, M.Map Name a)
emptyChain :: CState a
emptyChain = (0, M.empty)

type Error = String

-- \s -> [(Either e x, s)]
type Condition e v = EitherT e (StateT (CState v) [])
--type Condition a = (StateT (CState a) (EitherT Error []))

runCondition :: Condition e s a -> [(Either e a, CState s)]
runCondition m = runStateT (runEitherT m) emptyChain
--runCondition :: Condition s a -> [Either Error (a, CState s)]
--runCondition m = runEitherT $ runStateT m emptyChain

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

--amb :: Condition a b -> Condition a b -> Condition a b
--amb m1 m2 = StateT $ \s -> EitherT $ runEitherT (runStateT m1 s) ++ runEitherT (runStateT m2 s)
amb :: Condition e a b -> Condition e a b -> Condition e a b
amb m1 m2 = EitherT $ StateT $ \s -> 
  (runStateT (runEitherT m1) s) ++ (runStateT (runEitherT m2) s)



-- TODO is this good?
-- TODO use soft failure?
negative :: Condition e a b -> Condition e a ()
negative m = EitherT $ StateT $ \s0 ->
  runStateT (runEitherT m) s0 $>
  filter (isLeft . fst) $>
  map (\(_, s) -> (Right (), s))
 where
   ($>) = flip ($)

-- TODO 'fail' is not left :| not sure how to fix
exit :: e -> Condition e a b
-- Soft failure
exit = left

kill :: Condition e a ()
-- Hard failure, no record left
kill = EitherT (StateT (\s -> []))

assert :: Bool -> e -> Condition e a ()
assert cond msg =
  if cond then return () else exit msg

main = do
  x <- store 0 `amb` store 1 `amb` store 2
  y <- store 2
  val <- get x
  (assert (val == 0) $ show val)
  return (x, y)


