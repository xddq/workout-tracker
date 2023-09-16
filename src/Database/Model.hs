-- for deriving ToRow
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- to use "" for Text
{-# LANGUAGE OverloadedStrings #-}

-- export everything from our module
module Database.Model where

import Control.Exception (Exception)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Time (Day)
import Database.PostgreSQL.Simple.FromField (Field (typeOid), FromField (..), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import GHC.Generics (Generic)

-- We add an exception for the case "trying to delete rowsfrom the
-- database and deleting 0 rows" and "trying to update rows and updating 0 rows"
data CustomDbException = NoDeletedRows Text | FailedUpdate Text | FailedCreate Text deriving (Exception)

instance Show CustomDbException where
  show (NoDeletedRows err) = show $ "Deleted a row failed. More info: " <> err
  show (FailedUpdate err) = show $ "Updating a row failed. More info: " <> err
  show (FailedCreate err) = show $ "Creating a row failed. More info: " <> err

type ExerciseId = Int

type ExerciseTitle = Text

type ExerciseReps = [Int]

type ExerciseNote = Text

type ExercisePosition = Int

type ExerciseWorkoutId = Int

type ExerciseWeightsInKg = [Int]

-- Using smart constructor to create an exercise so that we avoid using PGArray
-- in other parts of the app besides the database.
-- The smart constructor also verifies (and if possible, fixes instead of fails)
-- that the input format is correct (length of reps list is equal to length of
-- weights list).
mkExercise :: ExerciseId -> ExerciseTitle -> ExerciseReps -> ExerciseNote -> ExercisePosition -> ExerciseWorkoutId -> ExerciseWeightsInKg -> Either Text Exercise
mkExercise id title reps note position workoutId weightsInKg = do
  -- Matching on "Right" case of the parseRepsAndWeights to enable short
  -- circuiting with the error in case the parsing failed.
  -- TODO: do we have to use the parseReps and parsedWeights in order to have
  -- the short circuiting? Or is using a tuple with matchall (_,_) enough?
  (parsedReps, parsedWeights) <- parseRepsAndWeights reps weightsInKg
  return $ Exercise id title (PGArray parsedReps) note position workoutId (PGArray parsedWeights)

-- Ensures that if we pass a different amount of sets based on reps and weights,
-- we get an error. If we pass for one of them a list with a single value, we
-- assume that the amount of reps or weights was used for the whole set and
-- adapt it accordingly.
parseRepsAndWeights :: ExerciseReps -> ExerciseWeightsInKg -> Either Text (ExerciseReps, ExerciseWeightsInKg)
parseRepsAndWeights reps@[x] weights@(y : ys) = Right (replicate (length weights) x, weights)
parseRepsAndWeights reps@(x : xs) weights@[y] = Right (reps, replicate (length reps) y)
parseRepsAndWeights reps weights = if length reps == length weights then Right (reps, weights) else Left "The amount of sets (based on reps) was not equal to the amount of sets (based on weights)"

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

mkCreateExerciseInput :: ExerciseTitle -> ExerciseReps -> ExerciseNote -> ExercisePosition -> ExerciseWorkoutId -> ExerciseWeightsInKg -> Either Text CreateExerciseInput
mkCreateExerciseInput title reps note position workoutId weights = do
  (parsedReps, parsedWeights) <- parseRepsAndWeights reps weights
  return $ CreateExerciseInput title (PGArray parsedReps) note position workoutId (PGArray parsedWeights)

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
    createWorkoutInputPrefillWorkoutId :: Int,
    createWorkoutInputNote :: Text
  }
  deriving (Show, Generic, FromRow, ToRow)
