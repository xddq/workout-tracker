-- for deriving ToRow
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- for allowing to create an instance for FromRow with [Int] instead of [a]
{-# LANGUAGE FlexibleInstances #-}
-- to use "" for Text
{-# LANGUAGE OverloadedStrings #-}

-- export everything from our module
module Database.Model where

import Control.Exception (Exception)
import qualified Data.ByteString as B
import Data.Text.Lazy (Text, pack, unpack)
import Data.Time (Day)
import Database.PostgreSQL.Simple.FromField (Conversion, Field (typeOid), FromField (..), ResultError (Incompatible, UnexpectedNull), TypeInfo (typoid), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.TypeInfo.Static (array_int4, int4)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray, fromPGArray))
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
  return $ Exercise id title parsedReps note position workoutId parsedWeights

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
    exerciseReps :: [Int],
    exerciseNote :: Text,
    exercisePosition :: Int,
    exerciseWorkoutId :: Int,
    exerciseWeightsInKg :: [Int]
  }
  deriving (Show, Generic)

instance ToRow Exercise where
  toRow exercise =
    [ toField (exerciseId exercise),
      toField (exerciseTitle exercise),
      toField (exerciseReps exercise),
      toField (exerciseNote exercise),
      toField (exercisePosition exercise),
      toField (exerciseWorkoutId exercise),
      toField (exerciseWeightsInKg exercise)
    ]

-- NOTE: Order matters here. SQL statements also have to return exercises with columns in this order
instance FromRow Exercise where
  fromRow =
    Exercise
      <$> field -- exerciseId
      <*> field -- exerciseTitle
      <*> field -- exerciseReps
      <*> field -- exerciseNote
      <*> field -- exercisePosition
      <*> field -- exerciseWorkoutId
      <*> field -- exerciseWeightsInKg

instance FromField [Int] where
  fromField f mdata =
    -- check if type is what we expect it to be
    if typeOid f /= typoid array_int4
      then returnError Incompatible f ""
      else do
        pgArray <- fromField f mdata :: Conversion (PGArray Int)
        return $ fromPGArray pgArray

instance ToField [Int] where
  -- wrap with PGArray when storing [Int] in db
  toField = toField . PGArray

mkCreateExerciseInput :: ExerciseTitle -> ExerciseReps -> ExerciseNote -> ExercisePosition -> ExerciseWorkoutId -> ExerciseWeightsInKg -> Either Text CreateExerciseInput
mkCreateExerciseInput title reps note position workoutId weights = do
  (parsedReps, parsedWeights) <- parseRepsAndWeights reps weights
  return $ CreateExerciseInput title (PGArray parsedReps) note position workoutId (PGArray parsedWeights)

-- NOTE: Could also use custom instances with [Int] instead of PGArray Int here.
-- Currently not needed since we don't operate on this data in other logic
-- layers of our app (e.g. views, webapp/routing). If needed, just do similar to
-- Exercise above.
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
