{-# LANGUAGE OverloadedStrings #-}

module Controllers.Workout where

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Controllers.Util
import Data.Text.Lazy (Text, pack, unpack)
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
readWorkout conn = do
  unparsedId <- param "id"
  -- success is an optional param. therefore using rescue to avoid throwing the
  -- exception up in case the optional param was not given.
  success <- param "success" `rescue` (\_ -> return False)
  exercisesWorkoutEither <- runExceptT $ do
    workoutId <- ExceptT $ pure $ textToEitherInt unparsedId
    workout <- ExceptT $ liftIO $ DB.getWorkoutById conn workoutId
    exercises <- ExceptT $ liftIO $ DB.getExercisesForWorkout conn (DB.workoutId workout)
    return (workout, exercises)
  either displayErrorPage (displayWorkoutPage success) exercisesWorkoutEither
  where displayWorkoutPage success (workout,exercises) = displayPage $ showWorkoutPage success workout exercises

updateWorkout :: Connection -> ActionM ()
updateWorkout conn = do
  workoutEither <- runExceptT $ do
    workoutId <- ExceptT $ textToEitherInt <$> param "id"
    ExceptT $ liftIO $ DB.getWorkoutById conn workoutId
  either displayErrorPage (displayPage . editWorkoutPage) workoutEither

deleteWorkout :: Connection -> ActionM ()
deleteWorkout conn = do
  workoutEither <- runExceptT $ do
    workoutId <- ExceptT $ textToEitherInt <$> param "id"
    ExceptT $ liftIO $ DB.getWorkoutById conn workoutId
  either displayErrorPage (displayPage . deleteWorkoutPage) workoutEither

orderWorkoutExercises :: Connection -> ActionM ()
orderWorkoutExercises conn = do
  exercisesEither <- runExceptT $ do
    workoutId <- ExceptT $ textToEitherInt <$> param "id"
    ExceptT $ liftIO (DB.getExercisesForWorkout conn workoutId)
  either displayErrorPage (displayPage . showOrderExercisesPage) exercisesEither

-- API controllers are called when we trying to mutate the database/state.
apiCreateWorkout :: Connection -> ActionM ()
apiCreateWorkout conn = do
  workoutType <- param "type"
  unparsedWorkoutId <- param "prefillWorkoutId"
  unparsedWorkoutDate <- param "date"
  workoutNote <- param "note"
  workoutEither <- runExceptT $ do
    (workoutId, date) <- ExceptT $ pure $ parseInput unparsedWorkoutId unparsedWorkoutDate
    ExceptT $ liftIO $ DB.createWorkout conn (DB.CreateWorkoutInput (if T.null workoutType then "Keine Angabe" else workoutType) date workoutId workoutNote)
  either displayErrorPage displayWorkoutPage workoutEither
  where
    displayWorkoutPage workout = redirect ("/workouts/" <> pack (show $ DB.workoutId workout) <> "/show?success=true")
    parseInput :: Text -> Text -> Either Text (Int, Day)
    parseInput id date = do
      (,) <$> textToEitherInt id <*> textToDate date

apiUpdateWorkout :: Connection -> ActionM ()
apiUpdateWorkout conn = do
  workoutType <- param "type"
  workoutNote <- param "note"
  unparsedWorkoutId <- param "id"
  unparsedWorkoutDate <- param "date"
  workoutEither <- runExceptT $ do
    (workoutId, date) <- ExceptT $ pure $ parseInput unparsedWorkoutId unparsedWorkoutDate
    ExceptT $ liftIO $ DB.updateWorkout conn (DB.Workout workoutId workoutType date workoutNote)
  either displayErrorPage displayLandingPage workoutEither
  where
    displayLandingPage _ = redirect "/?success=true"
    parseInput :: Text -> Text -> Either Text (Int, Day)
    parseInput id date = (,) <$> textToEitherInt id <*> textToDate date

apiDeleteWorkout :: Connection -> ActionM ()
apiDeleteWorkout conn = do
  unparsedWorkoutId <- param "workoutId"
  deletedRowsEither <- runExceptT $ do
    workoutId <- ExceptT $ pure $ textToEitherInt unparsedWorkoutId
    ExceptT $ liftIO $ DB.deleteWorkoutWithExercises conn workoutId
  either displayErrorPage displayLandingPage deletedRowsEither
  where
    displayErrorPage = displayPage . errorPage
    displayLandingPage _ = redirect ("/" <> "?success=true")
