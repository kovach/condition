module Monad.Condition
  ( Name, Condition
  , get, store, put, filterEnv, modifyAll
  , printEnv
  ) where

import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map as M
import Control.Monad.Trans.State hiding (get, put)
import qualified Control.Monad.Trans.State as S (get, put)

newtype Name = N Integer
 deriving (Eq, Ord)

instance Show Name where
  show (N i) = "#" ++ show i

type UState a = (Integer, M.Map Name a)
emptyChain :: UState a
emptyChain = (0, M.empty)

type Condition a = StateT (UState a) [] 
runCondition :: Condition s a -> [(a, UState s)]
runCondition m = runStateT m emptyChain

get :: Name -> Condition a a
get name = do
  (_, m) <- S.get
  return $ fromJust . M.lookup name $ m

store :: a -> Condition a Name
store val = do
  (count, env) <- S.get
  let name = N (count + 1)
  S.put (count + 1, M.insert name val env)
  return name

put :: Name -> a -> Condition a ()
put name val = modify $ \(c, env) -> (c, M.insert name val env)

-- Returns all objects that refer to given name
filterEnv :: (Name -> a -> Maybe b) -> Condition a [b]
filterEnv fn = do
  (_, env) <- S.get
  return $ mapMaybe (uncurry fn) $ M.toList env

modifyAll :: (Name -> a -> a) -> Condition a ()
modifyAll fn = do
  (c, env) <- S.get
  S.put (c, M.mapWithKey fn env)

printEnv :: Show a => Condition a String
printEnv = do
  (_, env) <- S.get
  return (unlines $ map show (M.toList env))

