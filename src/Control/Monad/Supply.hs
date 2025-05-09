{-# LANGUAGE UndecidableInstances #-}
-- |
-- Fresh variable supply
--
module Control.Monad.Supply where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader, MonadTrans)
import Control.Monad (MonadPlus)
import Control.Monad.State (StateT(..), MonadState(..))
import Control.Monad.Writer (MonadWriter)
import Data.Int (Int64)

import Data.Functor.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r, Alternative, MonadPlus, MonadIO)

runSupplyT :: Int64 -> SupplyT m a -> m (a, Int64)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Int64 -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Int64 -> Supply a -> (a, Int64)
runSupply n = runIdentity . runSupplyT n

instance MonadState s m => MonadState s (SupplyT m) where
  get = lift get
  put = lift . put
