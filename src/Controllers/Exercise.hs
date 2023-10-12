{-# LANGUAGE OverloadedStrings #-}

module Controllers.Exercise where

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Controllers.Util
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Text.Lazy (Text, pack, split, unpack)
import qualified Data.Text.Lazy as T
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import qualified Database.DB as DB
import qualified Database.Model as Controllers
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (status400)
import Text.Blaze.Html (Html)
import Views.Page
import Web.Scotty (ActionM, Param, defaultHandler, param, params, readEither, redirect, rescue, setHeader, status, text)
import Web.Scotty.Internal.Types (ActionT, ScottyError, ScottyT)

updateExercise :: Connection -> ActionM ()
updateExercise conn = do
  exerciseEither <- runExceptT $ do
    exerciseId <- ExceptT $ textToEitherInt <$> param "id"
    ExceptT $ liftIO $ DB.getExerciseById conn exerciseId
    -- NOTE: how it would look like with explicit return
    -- exercise <- ExceptT $ liftIO $ DB.getExerciseById conn exerciseId
    -- return exercise
  either displayErrorPage (displayPage . editExercisePage) exerciseEither

deleteExercise :: Connection -> ActionM ()
deleteExercise conn = do
  exerciseEither <- runExceptT $ do
    exerciseId <- ExceptT $ textToEitherInt <$> param "id"
    ExceptT $ liftIO $ DB.getExerciseById conn exerciseId
  either displayErrorPage (displayPage . deleteExercisePage) exerciseEither

-- API controllers are called when we trying to mutate the database/state.
apiCreateExercise :: Connection -> ActionM ()
apiCreateExercise conn = do
  title <- param "title"
  note <- param "note"
  unparsedReps <- param "reps"
  unparsedWeights <- param "weightsInKg"
  unparsedWorkoutId <- param "workoutId"
  exerciseEither <- runExceptT $ do
    (reps, weights, workoutId) <- ExceptT $ pure $ parseInput unparsedReps unparsedWeights unparsedWorkoutId
    position <- ExceptT $ liftIO $ DB.getHighestPositionByWorkoutId conn workoutId
    createExerciseInput <- ExceptT $ pure $ DB.mkCreateExerciseInput title reps note position workoutId weights
    ExceptT $ liftIO $ DB.createExercise conn createExerciseInput
  either (displayPage . errorPage) displayWorkoutPage exerciseEither
  where
    displayWorkoutPage exercise = redirect ("/workouts/" <> pack (show $ DB.exerciseWorkoutId exercise) <> "/show?success=true")
    parseInput :: Text -> Text -> Text -> Either Text ([Int], [Int], Int)
    parseInput unparsedReps unparsedWeights unparsedWorkoutId = do
      (,,) <$> textToEitherIntList unparsedReps <*> textToEitherIntList unparsedWeights <*> textToEitherInt unparsedWorkoutId

apiUpdateExercisePositions :: Connection -> ActionM ()
apiUpdateExercisePositions conn = do
  -- we pass position(first) exerciseId(second) multiple times (once per
  -- exercise) and read them out via 'params'.
  positions <- params
  let exercisePositionTuples = ensureAscendingPositions $ parsePositionExerciseIdTuples positions
   in do
        resultEither <- liftIO $ DB.updatePositionsOfExercises conn exercisePositionTuples
        either displayErrorPage displayWorkout resultEither
  where
    displayErrorPage = displayPage . errorPage
    displayWorkout nonEmptyExerciseList = redirect ("/workouts/" <> pack (show $ DB.exerciseWorkoutId $ NE.head nonEmptyExerciseList) <> "/show?success=true")

apiUpdateExercise :: Connection -> ActionM ()
apiUpdateExercise conn = do
  let parseInput :: Text -> Text -> Text -> Text -> Text -> Either Text (Int, [Int], Int, [Int], Int)
      parseInput id reps position weights workoutId = (,,,,) <$> textToEitherInt id <*> textToEitherIntList reps <*> textToEitherInt position <*> textToEitherIntList weights <*> textToEitherInt workoutId
  unparsedExerciseId <- param "id"
  title <- param "title"
  unparsedReps <- param "reps"
  note <- param "note"
  unparsedPosition <- param "position"
  unparsedWeights <- param "weightsInKg"
  unparsedWorkoutId <- param "workoutId"
  updatedExerciseEither <- runExceptT $ do
    (id, reps, position, weights, workoutId) <- ExceptT $ pure $ parseInput unparsedExerciseId unparsedReps unparsedPosition unparsedWeights unparsedWorkoutId
    exercise <- ExceptT $ pure $ DB.mkExercise id title reps note position workoutId weights
    ExceptT $ liftIO $ DB.updateExercise conn exercise
  either displayErrorPage displayWorkout updatedExerciseEither
  where
    displayErrorPage = displayPage . errorPage
    displayWorkout updatedExercise = redirect ("/workouts/" <> pack (show $ DB.exerciseWorkoutId updatedExercise) <> "/show?success=true")

apiDeleteExercise :: Connection -> ActionM ()
apiDeleteExercise conn = do
  deletedExerciseEither <- runExceptT $ do
    exerciseId <- ExceptT $ textToEitherInt <$> param "id"
    ExceptT $ liftIO $ DB.deleteExerciseById conn exerciseId
  either displayErrorPage displayWorkout deletedExerciseEither
  where
    parseInput :: Text -> Text -> Text -> Text -> Text -> Either Text (Int, [Int], Int, [Int], Int)
    parseInput exerciseId reps position weights workoutId = (,,,,) <$> textToEitherInt exerciseId <*> textToEitherIntList reps <*> textToEitherInt position <*> textToEitherIntList weights <*> textToEitherInt workoutId
    displayWorkout exercise = redirect ("/workouts/" <> pack (show $ DB.exerciseWorkoutId exercise) <> "/show?success=true")

type Position = Int

type ExerciseId = Int

-- TODO: fix parsing arguments. At least use Either and check the input format.
-- Ensures that exercise positions start with 1 and that every exercise with a
-- given position N has a subsequent position N+1 (or it is the last item of the
-- list :p).
ensureAscendingPositions :: [(Position, ExerciseId)] -> [(Position, ExerciseId)]
ensureAscendingPositions xs = zipWith (\newPosition (_, exerciseId) -> (newPosition, exerciseId)) [1 ..] (sortOn fst xs)

parsePositionExerciseIdTuples :: [Param] -> [(Position, ExerciseId)]
parsePositionExerciseIdTuples ((position, pValue) : (exerciseId, eValue) : xs) = (toInt pValue, toInt eValue) : parsePositionExerciseIdTuples xs
  where
    toInt x = read $ unpack x :: Int
parsePositionExerciseIdTuples [] = []
