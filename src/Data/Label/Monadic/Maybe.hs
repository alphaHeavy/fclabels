{-# LANGUAGE TypeOperators #-}
module Data.Label.Monadic.Maybe
(
-- * 'MonadState' lens operations.
  get

-- * 'MonadReader' lens operations.
, ask
)
where

import Control.Monad
import Data.Label.Maybe ((:~>))
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State  as M
import qualified Data.Label.Maybe     as L

-- | Get a value out of state, pointed to by the specified lens that might
-- fail.  When the lens getter fails this computation will fall back to
-- `mzero'.

get :: (M.MonadState f m, MonadPlus m) => (f :~> b) -> m b
get l = (L.get l `liftM` M.get) >>= (mzero `maybe` return)

-- | Fetch a value, pointed to by a lens that might fail, out of a reader
-- environment. When the lens getter fails this computation will fall back to
-- `mzero'.

ask :: (M.MonadReader f m, MonadPlus m) => (f :~> a) -> m a
ask l = (L.get l `liftM` M.ask) >>= (mzero `maybe` return)

