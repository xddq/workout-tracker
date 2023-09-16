-- to use "" for Text
{-# LANGUAGE OverloadedStrings #-}

module Database.DB
  ( createWorkout,
    getWorkouts,
    getWorkoutById,
    updateWorkout,
    CreateWorkoutInput (CreateWorkoutInput, createWorkoutInputDate, createWorkoutInputType),
    Workout (Workout, workoutId, workoutDate, workoutType),
    mkExercise,
    Exercise,
    exerciseId,
    exerciseTitle,
    exerciseReps,
    exerciseNote,
    exercisePosition,
    exerciseWorkoutId,
    exerciseWeightsInKg,
    getHighestPositionByWorkoutId,
    getExercisesForWorkout,
    getExerciseById,
    deleteExerciseById,
    updatePositionsOfExercises,
    deleteWorkoutWithExercises,
    mkCreateExerciseInput,
    CreateExerciseInput,
    createExerciseInputTitle,
    createExerciseInputReps,
    createExerciseInputNote,
    createExerciseInputPosition,
    createExerciseInputExerciseWorkoutId,
    createExerciseInputWeightsInKg,
    createExercise,
    updateExercise,
    repsToText,
    weightsToText,
  )
where

import Control.Exception (Exception, Handler (Handler), SomeException (SomeException), catch, catches, throw)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (find, sortBy, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Time (Day)
import Database.Model
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ResultError (ConversionFailed), SomePostgreSqlException (SomePostgreSqlException), ToRow, execute, executeMany, query, query_, returning, withTransaction)
import Database.PostgreSQL.Simple.FromField (Field (typeOid), FromField (..), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.TypeInfo
import Database.PostgreSQL.Simple.TypeInfo.Static (int4Oid, typoid)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Database.Util
import GHC.Generics (Generic)

unsafeGetWorkoutById :: Connection -> Int -> IO (Either Text Workout)
unsafeGetWorkoutById conn x = do
  workout <- query conn "SELECT * FROM workouts WHERE id = ?" (Only x) :: IO [Workout]
  case listToMaybe workout of
    Nothing -> return $ Left "No workout found for given id"
    Just x -> return $ Right x

getWorkoutById :: Connection -> Int -> IO (Either Text Workout)
getWorkoutById conn id = catchDbExceptions (unsafeGetWorkoutById conn id)

unsafeUpdateWorkout :: Connection -> Workout -> IO (Either Text Workout)
unsafeUpdateWorkout conn (Workout workoutId workoutType workoutDate workoutNote) = do
  workoutList <- query conn "UPDATE workouts SET type=?, date=?, note=? WHERE id=? RETURNING *" (workoutType, workoutDate, workoutNote, workoutId) :: IO [Workout]
  return $ listToEither "error updating workout" workoutList

updateWorkout :: Connection -> Workout -> IO (Either Text Workout)
updateWorkout conn workout = catchDbExceptions (unsafeUpdateWorkout conn workout)

unsafeGetExercisesForWorkout :: Connection -> Int -> IO (Either Text [Exercise])
unsafeGetExercisesForWorkout conn workoutId = do
  exercises <- query conn "SELECT * FROM exercises WHERE workout_id = ? ORDER BY position ASC" (Only workoutId)
  return $ Right exercises

getExercisesForWorkout :: Connection -> Int -> IO (Either Text [Exercise])
getExercisesForWorkout conn id = catchDbExceptions (unsafeGetExercisesForWorkout conn id)

unsafeGetExerciseById :: Connection -> Int -> IO (Either Text Exercise)
unsafeGetExerciseById conn id = do
  exercise <- query conn "SELECT * FROM exercises WHERE id = ?" (Only id)
  return $ maybeToEither "no exercise was found" $ listToMaybe exercise

getExerciseById :: Connection -> Int -> IO (Either Text Exercise)
getExerciseById conn id = catchDbExceptions (unsafeGetExerciseById conn id)

{- Deletes the given exercises and updates the positions of all exercises that
 - are left for the given workout. Ensures that exercises always have 'position'
 - from 1 to N where N is the last element of the list of exercises. Executed
 - within a transaction. If anything fails, a rollback is run (due to
 - withTransaction).
 -}
unsafeDeleteExerciseById :: Connection -> Exercise -> IO (Either Text Exercise)
unsafeDeleteExerciseById conn x = withTransaction conn $ do
  deletedExercises <- query conn "DELETE FROM exercises WHERE id = ? RETURNING *" (Only $ exerciseId x) :: IO [Exercise]
  case listToMaybe deletedExercises of
    -- throwing exception here for automated rollback
    Nothing -> throw $ NoDeletedRows $ "Deleting exercise with id: " <> pack (show $ exerciseId x) <> " failed"
    Just deletedExercise -> do
      Right exercises <- getExercisesForWorkout conn $ exerciseWorkoutId x
      updatedExercises <- mapM (updateExercise conn) (updatePriority $ sortExercises exercises)
      case find isLeft updatedExercises of
        -- throwing exception here for automated rollback
        Just (Left err) -> throw $ FailedUpdate err
        _ -> return $ Right deletedExercise
  where
    sortExercises = sortOn exercisePosition
    -- TODO: how could we write this in a shorter notation? I want to create a
    -- new Exercise with only an updated "position".
    -- Takes a sorted list of Exercises and updates the "position" attribute of
    -- each element according to its index in the list. E.g. first entry ->
    -- position 1, second entry -> position 2 etc..
    updatePriority = zipWith (\pos x -> Exercise (exerciseId x) (exerciseTitle x) (exerciseReps x) (exerciseNote x) pos (exerciseWorkoutId x) (exerciseWeightsInKg x)) [1 ..]

deleteExerciseById :: Connection -> Exercise -> IO (Either Text Exercise)
deleteExerciseById conn x = catchDbExceptions (unsafeDeleteExerciseById conn x)

unsafeDeleteWorkoutWithExercises :: Connection -> Int -> IO (Either Text Int64)
unsafeDeleteWorkoutWithExercises conn workoutId = withTransaction conn $ do
  execute conn "DELETE FROM exercises WHERE workout_id = ?" (Only workoutId)
  deletedRowCount <- execute conn "DELETE FROM workouts WHERE id = ?" (Only workoutId)
  -- throwing exception here for automated rollback
  if deletedRowCount == 0 then throw $ NoDeletedRows $ "Deleting workout with id: " <> pack (show workoutId) <> " failed." else return $ Right deletedRowCount

deleteWorkoutWithExercises :: Connection -> Int -> IO (Either Text Int64)
deleteWorkoutWithExercises conn workoutId = catchDbExceptions (unsafeDeleteWorkoutWithExercises conn workoutId)

-- using Either Text [Workout] here to be able to use the catchDbExceptions
-- function similar to the other functions
unsafeGetWorkouts :: Connection -> IO (Either Text [Workout])
unsafeGetWorkouts conn = do
  workouts <- query_ conn "SELECT * FROM workouts ORDER BY date DESC" :: IO [Workout]
  return $ Right workouts

getWorkouts :: Connection -> IO (Either Text [Workout])
getWorkouts conn = catchDbExceptions (unsafeGetWorkouts conn)

-- When creating an exercise the user should not think about its position, it is
-- rather just appended to the end of the list of exercises.
unsafeGetHighestPositionByWorkoutId :: Connection -> Int -> IO (Either Text Int)
unsafeGetHighestPositionByWorkoutId conn workoutId = do
  -- Use coalesce to avoid NULL return value if we have no exercises for a given
  -- workout.
  highestPosition <- query conn "SELECT coalesce(MAX(position),0) AS max FROM exercises WHERE workout_id=?" (Only workoutId) :: IO [Only Int]
  case listToMaybe highestPosition of
    Just (Only position) -> return $ Right $ position + 1
    _ -> return $ Left $ "Unable to find the max position based on the workout_id. workout_id was: " <> pack (show workoutId)

getHighestPositionByWorkoutId :: Connection -> Int -> IO (Either Text Int)
getHighestPositionByWorkoutId conn workoutId = catchDbExceptions (unsafeGetHighestPositionByWorkoutId conn workoutId)

unsafeCreateExercise :: Connection -> CreateExerciseInput -> IO (Either Text Exercise)
unsafeCreateExercise conn (CreateExerciseInput title reps note position workoutId weights) = do
  exerciseList <- query conn "INSERT INTO exercises (title, reps, note, position, workout_id, weight_in_kg) VALUES (?,?,?,?,?,?) RETURNING *" (title, reps, note, position, workoutId, weights)
  return $ listToEither "error creating exercise" exerciseList

createExercise :: Connection -> CreateExerciseInput -> IO (Either Text Exercise)
createExercise conn x = catchDbExceptions (unsafeCreateExercise conn x)

unsafeUpdateExercise :: Connection -> Exercise -> IO (Either Text Exercise)
unsafeUpdateExercise conn (Exercise id title reps note position workoutId weights) = do
  exerciseList <- query conn "UPDATE exercises SET title=?, reps=?, note=?, position=?, workout_id=?, weight_in_kg=?  WHERE id=? RETURNING *" (title, reps, note, position, workoutId, weights, id) :: IO [Exercise]
  return $ listToEither "error updating the exercise" exerciseList

updateExercise :: Connection -> Exercise -> IO (Either Text Exercise)
updateExercise conn x = catchDbExceptions (unsafeUpdateExercise conn x)

unsafeCreateWorkout :: Connection -> CreateWorkoutInput -> IO (Either Text Workout)
unsafeCreateWorkout conn x = withTransaction conn $ do
  createdWorkout <- query conn "INSERT INTO workouts (type, date, note) VALUES (?,?,?) RETURNING *" (createWorkoutInputType x, createWorkoutInputDate x, createWorkoutInputNote x) :: IO [Workout]
  case listToEither "Creating workout failed" createdWorkout of
    Left err -> return $ Left err
    Right workout ->
      if createWorkoutInputPrefillWorkoutId x == -1
        then return $ Right workout
        else do
          -- short circuit by matching on Right and return the Left when we have no match (select failed)
          Right exercises <- getExercisesForWorkout conn (createWorkoutInputPrefillWorkoutId x)
          createdExercises <- mapM (createExercise conn) (map (exerciseToCreateExerciseInput $ workoutId workout) exercises)
          case find isLeft createdExercises of
            -- throwing exception here for automated rollback
            Just (Left err) -> throw $ FailedCreate err
            _ -> return $ Right workout
  where
    exerciseToCreateExerciseInput workoutId ex = CreateExerciseInput (exerciseTitle ex) (exerciseReps ex) (exerciseNote ex) (exercisePosition ex) workoutId (exerciseWeightsInKg ex)

createWorkout :: Connection -> CreateWorkoutInput -> IO (Either Text Workout)
createWorkout conn x = catchDbExceptions (unsafeCreateWorkout conn x)

-- TODO: later use newtype wrapper?
type Position = Int

-- Using NonEmpty list here in order to store our evidence (list contains > 0 elements) in the type to avoid having to recheck the result of this function for this case once again.
unsafeUpdatePositionsOfExercises :: Connection -> [(Position, ExerciseId)] -> IO (Either Text (NE.NonEmpty Exercise))
unsafeUpdatePositionsOfExercises conn xs = do
  exercises <- returning conn "UPDATE exercises SET position = upd.position FROM (VALUES (?,?)) as upd(position,id) WHERE exercises.id = upd.id RETURNING exercises.*" xs :: IO [Exercise]
  return $ maybeToEither "Updating did not update any exercise." $ NE.nonEmpty exercises

updatePositionsOfExercises :: Connection -> [(Position, ExerciseId)] -> IO (Either Text (NE.NonEmpty Exercise))
updatePositionsOfExercises conn xs = catchDbExceptions (unsafeUpdatePositionsOfExercises conn xs)
