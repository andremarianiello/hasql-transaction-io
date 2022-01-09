{-# LANGUAGE OverloadedStrings #-}

module Hasql.Private.TransactionIO where

-- base
import Control.Applicative

-- bytestring
import Data.ByteString (ByteString)

-- bytestring-tree-builder
import ByteString.TreeBuilder

-- transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Error.Class

-- unliftio-core
import Control.Monad.IO.Unlift

-- resourcet
import Control.Monad.Trans.Resource

-- hasql
import Hasql.Statement
import Hasql.Session
import qualified Hasql.Session as Session

-- hasql-streaming
import Hasql.Private.Session.UnliftIO
import Hasql.Private.Types
import qualified Hasql.Private.Statements as Statements

newtype TransactionIO a = TransactionIO (ReaderT Transaction Session a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError QueryError, MonadUnliftIO)

data Transaction = Transaction

instance Semigroup a => Semigroup (TransactionIO a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (TransactionIO a) where
  mempty = pure mempty

{-# INLINE run #-}
run :: TransactionIO a -> IsolationLevel -> Mode -> Deferrable -> Bool -> Session a
run (TransactionIO txio) isolation mode deferrable preparable = runResourceT $ do
  UnliftIO runInIO <- lift askUnliftIO
  (_, tx) <- allocate (runInIO $ startTransaction isolation mode deferrable preparable) (runInIO . commitTransaction preparable)
  lift $ runReaderT txio tx

{-# INLINE sql #-}
sql :: ByteString -> TransactionIO ()
sql = TransactionIO . lift . Session.sql

{-# INLINE statement #-}
statement :: params -> Statement params result -> TransactionIO result
statement params stmt = TransactionIO . lift $ Session.statement params stmt

{-# INLINE startTransaction #-}
startTransaction :: IsolationLevel -> Mode -> Deferrable -> Bool -> Session Transaction
startTransaction isolation mode deferrable prepare = do
  liftIO (putStrLn "starting transaction")
  Session.statement () (Statements.startTransaction isolation mode deferrable prepare)
  pure Transaction

{-# INLINE commitTransaction #-}
commitTransaction :: Bool -> Transaction -> Session ()
commitTransaction prepare Transaction = do
  liftIO (putStrLn "committing transaction")
  Session.statement () (Statements.commitTransaction prepare)

{-# INLINE rollbackTransaction #-}
rollbackTransaction :: Bool -> Transaction -> Session ()
rollbackTransaction prepare Transaction = do
  liftIO (putStrLn "rolling back transaction")
  Session.statement () (Statements.rollbackTransaction prepare)
