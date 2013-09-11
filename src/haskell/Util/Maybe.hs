module Util.Maybe(
  maybeZero
) where

import Control.Monad

maybeZero :: (MonadPlus m) => Maybe a -> m a
maybeZero = maybe mzero return
