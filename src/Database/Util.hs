module Database.Util where

import Control.Exception (Handler (Handler), catches)
import Data.Maybe (listToMaybe)
import Data.Text.Lazy (Text, pack, unpack)
import Database.Model
import Database.PostgreSQL.Simple (SomePostgreSqlException (SomePostgreSqlException))
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))

-- Mainly for unwrapping database results where we want to query an item by id
-- (e.g. a workout by id) and get a list of workouts as result. This list is
-- (given a correct "select by id" SQL statement) either empty or contains
-- exactly one item. When it is empty, we want to create a Left with the error
-- message from the first argument of this function. Simple wrapper around
-- `maybeToEither err $ listToMaybe`.
listToEither :: Text -> [a] -> Either Text a
listToEither err xs = maybeToEither err $ listToMaybe xs

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

-- Wrapper to catch db exceptions and instead return an Either
catchDbExceptions :: IO (Either Text a) -> IO (Either Text a)
catchDbExceptions f =
  catches
    f
    [ Handler handleCustomDbException,
      Handler handleSomePostgresqlException
    ]
  where
    handleCustomDbException :: CustomDbException -> IO (Either Text a)
    handleCustomDbException e = do
      let err = show e
      putStrLn err
      return $ Left $ pack err
    handleSomePostgresqlException :: SomePostgreSqlException -> IO (Either Text a)
    handleSomePostgresqlException e = do
      let err = show e
      putStrLn err
      return $ Left $ pack err
