{-# LANGUAGE OverloadedStrings #-}

module Controllers.Workout where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT(..), except)
import Controllers.Util
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as T
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import qualified Database.DB as DB
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (status400)
import Text.Blaze.Html (Html)
import Views.Page
import Web.Scotty (ActionM, defaultHandler, param, raise, readEither, redirect, rescue, setHeader, status, text)
import Web.Scotty.Internal.Types (ActionT, ScottyError, ScottyT)

readWorkout :: Connection -> ActionM ()
readWorkout conn = leftToErrorPage $ do
  unparsedId <- lift $ param "id"
  -- success is an optional param. therefore using rescue to avoid throwing the
  -- exception up in case the optional param was not given.
  success <- lift $ param "success" `rescue` (\_ -> return False)
  parsedWorkoutId <- except $ textToEitherInt unparsedId
  workout <- ExceptT $ liftIO (DB.getWorkoutById conn parsedWorkoutId)
  exercises <- ExceptT $ liftIO (DB.getExercisesForWorkout conn (DB.workoutId workout))
  lift $ displayPage $ showWorkoutPage success workout exercises

updateWorkout :: Connection -> ActionM ()
updateWorkout conn = leftToErrorPage $ do
  id <- ExceptT $ textToEitherInt <$> param "id"
  workout <- ExceptT $ liftIO (DB.getWorkoutById conn id)
  lift $ displayPage $ editWorkoutPage workout

deleteWorkout :: Connection -> ActionM ()
deleteWorkout conn = leftToErrorPage $ do
  workoutId <- ExceptT $ textToEitherInt <$> param "id"
  workout <- ExceptT $ liftIO (DB.getWorkoutById conn workoutId)
  lift $ displayPage $ deleteWorkoutPage workout

orderWorkoutExercises :: Connection -> ActionM ()
orderWorkoutExercises conn = leftToErrorPage $ do
  workoutId <- ExceptT $ textToEitherInt <$> param "id"
  exercises <- ExceptT $ liftIO (DB.getExercisesForWorkout conn workoutId)
  lift $ displayPage $ showOrderExercisesPage exercises

-- API controllers execute business logic and once done redirect to another
-- route. Api controllers 'raise' errors which will get caught by the default
-- error handler, which in turn will simplay display a page with the error. In
-- contrast to normal controllers, API controllers don't directly return html.
apiCreateWorkout :: Connection -> ActionM ()
apiCreateWorkout conn = do
  workoutType <- param "type"
  unparsedWorkoutId <- param "prefillWorkoutId"
  unparsedWorkoutDate <- param "date"
  workoutNote <- param "note"
  case parseInput unparsedWorkoutId unparsedWorkoutDate of
    Left err -> raise err
    Right (id, date) -> do
      createdWorkout <- liftIO $ DB.createWorkout conn (DB.CreateWorkoutInput (if T.null workoutType then "Keine Angabe" else workoutType) date id workoutNote)
      case createdWorkout of
        Right _ -> redirect ("/" <> "?success=true")
        Left err -> raise err
  where
    parseInput :: Text -> Text -> Either Text (Int, Day)
    parseInput id date = do
      (,) <$> textToEitherInt id <*> textToDate date

apiUpdateWorkout :: Connection -> ActionM ()
apiUpdateWorkout conn = do
  workoutType <- param "type"
  workoutNote <- param "note"
  unparsedWorkoutId <- param "id"
  unparsedWorkoutDate <- param "date"
  case parseInput unparsedWorkoutId unparsedWorkoutDate of
    Left err -> raise err
    Right (workoutId, date) -> do
      workoutEither <- liftIO $ DB.updateWorkout conn (DB.Workout workoutId workoutType date workoutNote)
      case workoutEither of
        Left err -> raise err
        Right workout -> redirect ("/" <> "?success=true")
  where
    parseInput :: Text -> Text -> Either Text (Int, Day)
    parseInput id date = (,) <$> textToEitherInt id <*> textToDate date

apiDeleteWorkout :: Connection -> ActionM ()
apiDeleteWorkout conn = do
  unparsedWorkoutId <- param "workoutId"
  case textToEitherInt unparsedWorkoutId of
    Left err -> displayPage $ errorPage err
    Right id -> do
      deletedRowsCountEither <- liftIO $ DB.deleteWorkoutWithExercises conn id
      case deletedRowsCountEither of
        Left err -> raise err
        Right deletedRowsCount -> redirect ("/" <> "?success=true")
