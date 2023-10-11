{-# LANGUAGE OverloadedStrings #-}

module Controllers.Exercise where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT(..), except)
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
import Web.Scotty (ActionM, Param, defaultHandler, param, params, raise, readEither, redirect, rescue, setHeader, status, text)
import Web.Scotty.Internal.Types (ActionT, ScottyError, ScottyT)

updateExercise :: Connection -> ActionM ()
updateExercise conn = leftToErrorPage $ do
  parsedExerciseId <- ExceptT $ textToEitherInt <$> param "id"
  exercise <- ExceptT $ liftIO (DB.getExerciseById conn parsedExerciseId)
  lift $ displayPage $ editExercisePage exercise

deleteExercise :: Connection -> ActionM ()
deleteExercise conn = leftToErrorPage $ do
  parsedExerciseId <- ExceptT $ textToEitherInt <$> param "id"
  exerciseEither <- ExceptT $ liftIO (DB.getExerciseById conn parsedExerciseId)
  lift $ displayPage $ deleteExercisePage exerciseEither

-- API controllers execute business logic and once done redirect to another
-- route. Api controllers 'raise' errors which will get caught by the default
-- error handler, which in turn will simplay display a page with the error. In
-- contrast to normal controllers, API controllers don't directly return html.
apiCreateExercise :: Connection -> ActionM ()
apiCreateExercise conn = do
  title <- param "title"
  note <- param "note"
  unparsedReps <- param "reps"
  unparsedWeights <- param "weightsInKg"
  unparsedWorkoutId <- param "workoutId"
  case parseInput unparsedReps unparsedWeights unparsedWorkoutId of
    Left err -> displayPage $ errorPage err
    Right (reps, weights, workoutId) -> leftToRaise $ do
      position <- ExceptT $ liftIO $ DB.getHighestPositionByWorkoutId conn workoutId
      createExerciseInput <- except $ DB.mkCreateExerciseInput title reps note position workoutId weights
      createdExercise <- ExceptT $ liftIO $ DB.createExercise conn createExerciseInput
      lift $ redirect ("/workouts/" <> pack (show $ DB.exerciseWorkoutId createdExercise) <> "/show?success=true")
  where
    parseInput :: Text -> Text -> Text -> Either Text ([Int], [Int], Int)
    parseInput unparsedReps unparsedWeights unparsedWorkoutId = do
      (,,) <$> textToEitherIntList unparsedReps <*> textToEitherIntList unparsedWeights <*> textToEitherInt unparsedWorkoutId

    leftToRaise (ExceptT e) = e >>= either raise pure

apiUpdateExercises :: Connection -> ActionM ()
apiUpdateExercises conn = do
  -- we pass position(first) exerciseId(second) multiple times (once per
  -- exercise) and read them out via 'params'.
  positions <- params
  let exercisePositionTuples = ensureAscendingPositions $ parsePositionExerciseIdTuples positions
   in do
        resultEither <- liftIO $ DB.updatePositionsOfExercises conn exercisePositionTuples
        case resultEither of
          Right exerciseList -> redirect ("/workouts/" <> pack (show $ DB.exerciseWorkoutId $ NE.head exerciseList) <> "/show?success=true")
          Left err -> raise err

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
  case parseInput unparsedExerciseId unparsedReps unparsedPosition unparsedWeights unparsedWorkoutId of
    Left err -> displayPage $ errorPage err
    Right (id, reps, position, weights, workoutId) -> do
      case DB.mkExercise id title reps note position workoutId weights of
        Left err -> displayPage $ errorPage err
        Right exercise -> do
          updatedExerciseEither <- liftIO $ DB.updateExercise conn exercise
          case updatedExerciseEither of
            Left err -> raise err
            Right updatedExercise -> redirect ("/workouts/" <> pack (show $ DB.exerciseWorkoutId updatedExercise) <> "/show?success=true")

apiDeleteExercise :: Connection -> ActionM ()
apiDeleteExercise conn = do
  unparsedExerciseId <- param "id"
  title <- param "title"
  unparsedReps <- param "reps"
  note <- param "note"
  unparsedPosition <- param "position"
  unparsedWeights <- param "weightsInKg"
  unparsedWorkoutId <- param "workoutId"
  case parseInput unparsedExerciseId unparsedReps unparsedPosition unparsedWeights unparsedWorkoutId of
    Left err -> raise err
    Right (exerciseId, reps, position, weights, workoutId) -> do
      case DB.mkExercise exerciseId title reps note position workoutId weights of
        Left err -> raise err
        Right exercise -> do
          deletedExerciseEither <- liftIO $ DB.deleteExerciseById conn exercise
          case deletedExerciseEither of
            Left err -> raise err
            Right deletedExercise -> redirect ("/workouts/" <> pack (show workoutId) <> "/show?success=true")
  where
    parseInput :: Text -> Text -> Text -> Text -> Text -> Either Text (Int, [Int], Int, [Int], Int)
    parseInput exerciseId reps position weights workoutId = (,,,,) <$> textToEitherInt exerciseId <*> textToEitherIntList reps <*> textToEitherInt position <*> textToEitherIntList weights <*> textToEitherInt workoutId

type Position = Int

type ExerciseId = Int

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
