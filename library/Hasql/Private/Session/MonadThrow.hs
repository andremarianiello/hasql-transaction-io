module Hasql.Private.Session.MonadThrow where

-- base
import           Control.Exception

-- transformers
import           Control.Monad.IO.Class

-- exceptions
import           Control.Monad.Catch    (MonadThrow (..))

--hasql
import           Hasql.Session

instance MonadThrow Session where
  throwM = liftIO . throwIO
