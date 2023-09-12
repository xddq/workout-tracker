-- for deriving ToRow
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- to use "" for Text
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( createWorkout,
    getWorkouts,
    getWorkoutById,
    updateWorkout,
    CreateWorkoutInput (CreateWorkoutInput, createWorkoutInputDate, createWorkoutInputType),
    Workout (Workout, workoutId, workoutDate, workoutType),
    Exercise (Exercise, exerciseId, exerciseTitle, exerciseReps, exerciseNote, exercisePosition, exerciseWorkoutId, exerciseWeightsInKg),
    getHighestPositionByWorkoutId,
    getExercisesForWorkout,
    getExerciseById,
    deleteExerciseById,
    updatePositionOfExercise,
    updatePositionsOfExercises,
    deleteWorkoutWithExercises,
    CreateExerciseInput (CreateExerciseInput, createExerciseInputTitle, createExerciseInputReps, createExerciseInputNote, createExerciseInputPosition, createExerciseInputExerciseWorkoutId, createExerciseInputWeightsInKg),
    createExercise,
    updateExercise,
    repsToText,
    weightsToText,
    maybeToRight,
  )
where

import Control.Exception (Exception, Handler (Handler), SomeException (SomeException), catch, catches, throw)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List (sortBy, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Time (Day)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ResultError (ConversionFailed), SomePostgreSqlException (SomePostgreSqlException), ToRow, execute, executeMany, query, query_, withTransaction)
import Database.PostgreSQL.Simple.FromField (Field (typeOid), FromField (..), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.TypeInfo
import Database.PostgreSQL.Simple.TypeInfo.Static (int4Oid, typoid)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import GHC.Generics (Generic)

-- We want to add an exception for the case "trying to delete something from the database and deleting 0 rows"
data CustomDbException = NoDeletedRows deriving (Show)

instance Exception CustomDbException

data Exercise = Exercise
  { exerciseId :: Int,
    exerciseTitle :: Text,
    exerciseReps :: PGArray Int,
    exerciseNote :: Text,
    exercisePosition :: Int,
    exerciseWorkoutId :: Int,
    exerciseWeightsInKg :: PGArray Int
  }
  deriving (Show, Generic, FromRow, ToRow)

repsToText :: PGArray Int -> String
repsToText (PGArray xs) = foldl (\acc curr -> if null acc then show curr else acc ++ "," ++ show curr) "" xs

weightsToText :: PGArray Int -> String
weightsToText = repsToText

data CreateExerciseInput = CreateExerciseInput
  { createExerciseInputTitle :: Text,
    createExerciseInputReps :: PGArray Int,
    createExerciseInputNote :: Text,
    createExerciseInputPosition :: Int,
    createExerciseInputExerciseWorkoutId :: Int,
    createExerciseInputWeightsInKg :: PGArray Int
  }
  deriving (Show, Generic, FromRow, ToRow)

data Workout = Workout
  { workoutId :: Int,
    workoutType :: Text,
    workoutDate :: Day,
    workoutNote :: Text
  }
  deriving (Show, Generic, FromRow, ToRow)

data CreateWorkoutInput = CreateWorkoutInput
  { createWorkoutInputType :: Text,
    createWorkoutInputDate :: Day,
    createWorkoutInputPrefillWorkoutId :: Int
  }
  deriving (Show, Generic, FromRow, ToRow)

-- TODO/MAYBE (low prio): Adapt all the other db call functions to also use an
-- unsafe version and adapt the normal version to return Either Text a. Did this
-- for some (e.g. unsafeDeleteWorkoutWithExercises) and understood the approach
-- and concept (which was my goal).
createWorkout :: Connection -> CreateWorkoutInput -> IO (Maybe Workout)
createWorkout conn x = do
  createdWorkout <- query conn "INSERT INTO workouts (type, date) VALUES (?,?) RETURNING *" (createWorkoutInputType x, createWorkoutInputDate x) :: IO [Workout]
  case listToMaybe createdWorkout of
    Nothing -> return Nothing
    Just workout -> do
      if createWorkoutInputPrefillWorkoutId x == -1
        then return (Just workout)
        else do
          Right exercises <- getExercisesForWorkout conn (createWorkoutInputPrefillWorkoutId x)
          mapM_ (createExercise conn) (map (exerciseToCreateExerciseInput $ workoutId workout) exercises)
          return $ Just workout
  where
    exerciseToCreateExerciseInput workoutId ex = CreateExerciseInput (exerciseTitle ex) (exerciseReps ex) (exerciseNote ex) (exercisePosition ex) workoutId (exerciseWeightsInKg ex)

-- TODO: perhaps move this into a util package..? Or decide which other
-- package to install and to import it from..?
maybeToRight :: Text -> Maybe a -> Either Text a
maybeToRight err Nothing = Left err
maybeToRight _ (Just x) = Right x

-- using Either Text [Workout] here to be able to use the catchDbExceptions
-- function similar to the other functions
unsafeGetWorkouts :: Connection -> IO (Either Text [Workout])
unsafeGetWorkouts conn = do
  workouts <- query_ conn "SELECT * FROM workouts ORDER BY date DESC" :: IO [Workout]
  return $ Right workouts

getWorkouts :: Connection -> IO (Either Text [Workout])
getWorkouts conn = catchDbExceptions (unsafeGetWorkouts conn)

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
  case listToMaybe workoutList of
    Just workout -> return $ Right workout
    Nothing -> return $ Left "error updating workout"

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
  return $ maybeToRight "no exercise was found" $ listToMaybe exercise

getExerciseById :: Connection -> Int -> IO (Either Text Exercise)
getExerciseById conn id = catchDbExceptions (unsafeGetExerciseById conn id)

-- TODO: start and stop transaction here
{- Deletes the given exercises and updates the positions of all exercises that
 - are left for the given workout. Ensures that exercises always have 'position'
 - from 1 to N where N is the last element of the list of exercises.
 -}
deleteExerciseById :: Connection -> Exercise -> IO (Maybe Exercise)
deleteExerciseById conn x = do
  deletedExercises <- query conn "DELETE FROM exercises WHERE id = ? RETURNING *" (Only $ exerciseId x) :: IO [Exercise]
  case listToMaybe deletedExercises of
    Nothing -> return Nothing
    Just deletedExercise -> do
      Right exercises <- getExercisesForWorkout conn $ exerciseWorkoutId x
      updatedExercises <- mapM (updateExercise conn) (updatePriority $ sortExercises exercises)
      return $ Just deletedExercise
  where
    sortExercises = sortOn exercisePosition
    -- TODO: how could we write this in a shorter notation? I want to create a
    -- new Exercise with only an updated "position".
    -- Takes a sorted list of Exercises and updates the "position" attribute of
    -- each element according to its index in the list. E.g. first entry ->
    -- position 1, second entry -> position 2 etc..
    updatePriority = zipWith (\pos x -> Exercise (exerciseId x) (exerciseTitle x) (exerciseReps x) (exerciseNote x) pos (exerciseWorkoutId x) (exerciseWeightsInKg x)) [1 ..]

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

unsafeDeleteWorkoutWithExercises :: Connection -> Int -> IO (Either Text Int64)
unsafeDeleteWorkoutWithExercises conn workoutId = withTransaction conn $ do
  execute conn "DELETE FROM exercises WHERE workout_id = ?" (Only workoutId)
  deletedRowCount <- execute conn "DELETE FROM workouts WHERE id = ?" (Only workoutId)
  -- throwing exception here for automated rollback
  if deletedRowCount == 0 then throw NoDeletedRows else return $ Right deletedRowCount

deleteWorkoutWithExercises :: Connection -> Int -> IO (Either Text Int64)
deleteWorkoutWithExercises conn workoutId = catchDbExceptions (unsafeDeleteWorkoutWithExercises conn workoutId)

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

-- The create exercise functions which throws exceptions.
unsafeCreateExercise :: Connection -> CreateExerciseInput -> IO (Either Text Exercise)
unsafeCreateExercise conn (CreateExerciseInput title reps note position workoutId weights) = do
  result <- query conn "INSERT INTO exercises (title, reps, note, position, workout_id, weight_in_kg) VALUES (?,?,?,?,?,?) RETURNING *" (title, reps, note, position, workoutId, weights)
  case listToMaybe result of
    Just exercise -> do
      print exercise
      return $ Right exercise
    Nothing -> do
      print "error"
      return $ Left "Error creating the exercise. The 'returning *' gave us an empty list."

-- trying out catching exceptions and using either type under the hood.
createExercise :: Connection -> CreateExerciseInput -> IO (Either Text Exercise)
createExercise conn x = catchDbExceptions (unsafeCreateExercise conn x)

unsafeUpdateExercise :: Connection -> Exercise -> IO (Either Text Exercise)
unsafeUpdateExercise conn (Exercise id title reps note position workoutId weights) = do
  result <- query conn "UPDATE exercises SET title=?, reps=?, note=?, position=?, workout_id=?, weight_in_kg=?  WHERE id=? RETURNING *" (title, reps, note, position, workoutId, weights, id) :: IO [Exercise]
  case listToMaybe result of
    Just exercise -> return $ Right exercise
    Nothing -> return $ Left "Error updating the exercise. The 'returning *' gave us an empty list."

updateExercise :: Connection -> Exercise -> IO (Either Text Exercise)
updateExercise conn x = catchDbExceptions (unsafeUpdateExercise conn x)

-- TODO: later use newtype wrapper
type Position = Int

type ExerciseId = Int

updatePositionOfExercise :: Connection -> (Position, ExerciseId) -> IO (Maybe Exercise)
updatePositionOfExercise conn (position, id) = do
  result <- query conn "UPDATE exercises SET position=? WHERE id=? RETURNING *" (position, id) :: IO [Exercise]
  return $ listToMaybe result

updatePositionsOfExercises :: Connection -> [(Position, ExerciseId)] -> IO (Maybe Int)
updatePositionsOfExercises conn xs = do
  rowsAffected <- executeMany conn "UPDATE exercises SET position = upd.position FROM (VALUES (?,?)) as upd(position,id) WHERE exercises.id = upd.id" xs :: IO Int64
  if rowsAffected == 0 then return Nothing else return $ Just $ fromIntegral rowsAffected
