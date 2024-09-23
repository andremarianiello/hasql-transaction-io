{-# LANGUAGE OverloadedStrings #-}

module Hasql.Private.TransactionIO where

-- base
import           Control.Applicative

-- bytestring
import           Data.ByteString                  (ByteString)

-- bytestring-tree-builder
import           ByteString.TreeBuilder

-- transformers
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader

-- mtl
import           Control.Monad.Error.Class

-- unliftio-core
import           Control.Monad.IO.Unlift

-- safe-exceptions
import           Control.Exception.Safe

-- resourcet
import           Control.Monad.Trans.Resource
import           Data.Acquire

-- hasql
import           Hasql.Session
import qualified Hasql.Session                    as Session
import           Hasql.Statement

-- hasql-streaming
import           Hasql.Private.Session.MonadThrow
import           Hasql.Private.Session.UnliftIO
import qualified Hasql.Private.Statements         as Statements
import           Hasql.Private.Types

-- | A mixture of Hasql statements and arbitrary IO that is all performed during a single transaction
newtype TransactionIO a = TransactionIO (ReaderT Transaction Session a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError SessionError, MonadUnliftIO, MonadThrow)

data Transaction = Transaction

instance Semigroup a => Semigroup (TransactionIO a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (TransactionIO a) where
  mempty = pure mempty

run :: TransactionIO a -> IsolationLevel -> Mode -> Deferrable -> Bool -> Session a
run (TransactionIO txio) isolation mode deferrable preparable = runResourceT $ do
  UnliftIO runInIO <- lift askUnliftIO
  let acq = mkAcquireType (runInIO $ startTransaction isolation mode deferrable preparable) ((runInIO .) . endTransaction preparable)
  (_, tx) <- allocateAcquire acq
  lift $ runReaderT txio tx

-- | Like `Session.sql` but in a `TransactionIO`. It should not attempt any statements that cannot be safely run inside a transaction.
sql :: ByteString -> TransactionIO ()
sql = TransactionIO . lift . Session.sql

-- | Like `Session.statement` but in a `TransactionIO`. It should not attempt any statements that cannot be safely run inside a transaction.
statement :: params -> Statement params result -> TransactionIO result
statement params stmt = TransactionIO . lift $ Session.statement params stmt

startTransaction :: IsolationLevel -> Mode -> Deferrable -> Bool -> Session Transaction
startTransaction isolation mode deferrable prepare = do
  Session.statement () (Statements.startTransaction isolation mode deferrable prepare)
  pure Transaction

endTransaction :: Bool -> Transaction -> ReleaseType -> Session ()
endTransaction prepare tx = \case
  ReleaseEarly     -> commitTransaction prepare tx
  ReleaseNormal    -> commitTransaction prepare tx
  ReleaseException -> rollbackTransaction prepare tx

commitTransaction :: Bool -> Transaction -> Session ()
commitTransaction prepare Transaction = do
  Session.statement () (Statements.commitTransaction prepare)

rollbackTransaction :: Bool -> Transaction -> Session ()
rollbackTransaction prepare Transaction = do
  Session.statement () (Statements.rollbackTransaction prepare)



data CondemnTransactionException = CondemnTransactionException
  deriving (Show)

instance Exception CondemnTransactionException

-- | Throw an internal exception that causes the transaction to be rolled back. If you wish to rollback a transaction with a more useful exception use `throwIO`
condemn :: TransactionIO a
condemn = throwIO CondemnTransactionException
