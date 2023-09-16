{-# LANGUAGE OverloadedStrings #-}

module Webapp (mkApp) where

import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (IdentityT, runIdentityT))
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), ToJSON (toJSON), Value, decode, encode, fromJSON, object, withObject, (.:), (.=))
import Data.Either (fromLeft, isLeft)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text, null, pack, split, unpack)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read (decimal)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Database.DB (CreateExerciseInput, CreateWorkoutInput (CreateWorkoutInput), Exercise, Workout (Workout, workoutId), createExercise, createWorkout, deleteExerciseById, deleteWorkoutWithExercises, exerciseWorkoutId, getExerciseById, getExercisesForWorkout, getHighestPositionByWorkoutId, getWorkoutById, getWorkouts, mkCreateExerciseInput, mkExercise, updateExercise, updatePositionsOfExercises, updateWorkout)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, status500)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Text.Blaze.Html (Html)
import Text.Read (readMaybe)
import Views.Page (deleteExercisePage, deleteWorkoutPage, editExercisePage, editWorkoutPage, errorPage, htmlToText, landingPage, mkCurrentDate, showOrderExercisesPage, showWorkoutPage, successPage)
import Web.Scotty (ActionM, Param, Parsable (parseParam), body, delete, get, html, middleware, param, params, patch, post, readEither, redirect, rescue, scottyApp, setHeader, status, text)

displayPage :: Html -> ActionM ()
displayPage x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  text $ htmlToText x

textToDate :: Text -> Either Text Day
textToDate txt = do
  let parsedMaybe = parseTimeM True defaultTimeLocale dateFormat $ unpack txt
  case parsedMaybe of
    Just day -> Right day
    Nothing -> Left $ "Could not parse: " <> txt
  where
    -- days may have 1 or 2 chars, then one space, then month with one or two
    -- chars then one space and a 4 char year. Example: 23 07 2023
    dateFormat :: String
    dateFormat = "%-d.%-m.%Y"

textToEitherInt :: Text -> Either Text Int
textToEitherInt = readEither

textToEitherIntList :: Text -> Either Text [Int]
textToEitherIntList txt = mapM readEither $ textToList txt
  where
    textToList :: Text -> [Text]
    textToList = split (== ',')
    readEitherToInt :: Text -> Either Text Int
    readEitherToInt = readEither

mkApp :: Connection -> IO Application
mkApp conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    -- log all requests in console
    middleware logStdoutDev
    -- serve static files from the "static" directory
    middleware $ staticPolicy (addBase "static")

    get "/" $ do
      success <- param "success" `rescue` (\_ -> return False)
      currentDate <- liftIO (utctDay <$> getCurrentTime)
      workoutsEither <- liftIO (getWorkouts conn)
      displayPage $ landingPage success (mkCurrentDate currentDate) workoutsEither

    get "/workouts/:id/edit" $ do
      unparsedId <- param "id"
      case textToEitherInt unparsedId of
        Left err -> displayPage $ errorPage err
        Right id -> do
          workoutEither <- liftIO (getWorkoutById conn id)
          displayPage $ editWorkoutPage workoutEither

    get "/workouts/:id/show" $ do
      unparsedId <- param "id"
      success <- param "success" `rescue` (\_ -> return False)
      case textToEitherInt unparsedId of
        Left err -> displayPage $ errorPage err
        Right parsedWorkoutId -> do
          workoutEither <- liftIO (getWorkoutById conn parsedWorkoutId)
          case workoutEither of
            Left err -> displayPage $ errorPage err
            Right workout -> do
              exercisesEither <- liftIO (getExercisesForWorkout conn (workoutId workout))
              displayPage $ showWorkoutPage success workout exercisesEither

    get "/workouts/:id/delete" $ do
      unparsedWorkoutId <- param "id"
      case textToEitherInt unparsedWorkoutId of
        Left err -> displayPage $ errorPage err
        Right workoutId -> do
          workoutEither <- liftIO (getWorkoutById conn workoutId)
          displayPage $ deleteWorkoutPage workoutEither

    get "/workouts/:id/exercises/order" $ do
      unparsedWorkoutId <- param "id"
      case textToEitherInt unparsedWorkoutId of
        Left err -> displayPage $ errorPage err
        Right workoutId -> do
          exercisesEither <- liftIO (getExercisesForWorkout conn workoutId)
          displayPage $ showOrderExercisesPage exercisesEither

    get "/exercises/:id/delete" $ do
      unparsedId <- param "id"
      case textToEitherInt unparsedId of
        Left err -> text $ htmlToText (errorPage err)
        Right parsedExerciseId -> do
          exerciseEither <- liftIO (getExerciseById conn parsedExerciseId)
          displayPage $ deleteExercisePage exerciseEither

    get "/exercises/:id/edit" $ do
      unparsedId <- param "id"
      case textToEitherInt unparsedId of
        Left err -> displayPage $ errorPage err
        Right parsedExerciseId -> do
          exerciseEither <- liftIO (getExerciseById conn parsedExerciseId)
          displayPage $ editExercisePage exerciseEither

    post "/api/create-workout" $ do
      let parseInput :: Text -> Text -> Either Text (Int, Day)
          parseInput id date = do
            (,) <$> textToEitherInt id <*> textToDate date
      workoutType <- param "type"
      unparsedWorkoutId <- param "prefillWorkoutId"
      unparsedWorkoutDate <- param "date"
      workoutNote <- param "note"
      case parseInput unparsedWorkoutId unparsedWorkoutDate of
        Left err -> displayPage $ errorPage err
        Right (id, date) -> do
          createdWorkout <- liftIO $ createWorkout conn (CreateWorkoutInput (if T.null workoutType then "Keine Angabe" else workoutType) date id workoutNote)
          case createdWorkout of
            Right _ -> redirect ("/" <> "?success=true")
            Left err -> displayPage $ errorPage err

    post "/api/update-workout" $ do
      let parseInput :: Text -> Text -> Either Text (Int, Day)
          parseInput id date = (,) <$> textToEitherInt id <*> textToDate date
      workoutType <- param "type"
      workoutNote <- param "note"
      unparsedWorkoutId <- param "id"
      unparsedWorkoutDate <- param "date"
      case parseInput unparsedWorkoutId unparsedWorkoutDate of
        Left err -> displayPage $ errorPage err
        Right (workoutId, date) -> do
          workoutEither <- liftIO $ updateWorkout conn (Workout workoutId workoutType date workoutNote)
          case workoutEither of
            Left err -> displayPage $ errorPage err
            Right workout -> redirect ("/" <> "?success=true")

    post "/api/delete-workout" $ do
      unparsedWorkoutId <- param "workoutId"
      case textToEitherInt unparsedWorkoutId of
        Left err -> displayPage $ errorPage err
        Right id -> do
          deletedRowsCount <- liftIO $ deleteWorkoutWithExercises conn id
          either
            (displayPage . errorPage)
            (\_ -> redirect ("/" <> "?success=true"))
            deletedRowsCount

    post "/api/create-exercise" $ do
      let parseInput :: Text -> Text -> Text -> Either Text ([Int], [Int], Int)
          parseInput unparsedReps unparsedWeights unparsedWorkoutId = do
            (,,) <$> textToEitherIntList unparsedReps <*> textToEitherIntList unparsedWeights <*> textToEitherInt unparsedWorkoutId
      title <- param "title"
      note <- param "note"
      unparsedReps <- param "reps"
      unparsedWeights <- param "weightsInKg"
      unparsedWorkoutId <- param "workoutId"
      case parseInput unparsedReps unparsedWeights unparsedWorkoutId of
        Left err -> displayPage $ errorPage err
        Right (reps, weights, workoutId) -> do
          position <- liftIO $ getHighestPositionByWorkoutId conn workoutId
          case position of
            Left err -> displayPage $ errorPage err
            Right position -> do
              case mkCreateExerciseInput title reps note position workoutId weights of
                Left err -> displayPage $ errorPage err
                Right createExerciseInput -> do
                  createdExercise <- liftIO $ createExercise conn createExerciseInput
                  either
                    (displayPage . errorPage)
                    (\x -> redirect ("/workouts/" <> pack (show $ exerciseWorkoutId x) <> "/show?success=true"))
                    createdExercise

    -- bulk updates exercises, used for updating their position/order
    post "/api/update-exercises" $ do
      -- we pass position(first) exerciseId(second) multiple times (once per
      -- exercise) and read them out via 'params'.
      positions <- params
      let exercisePositionTuples = ensureAscendingPositions $ parsePositionExerciseIdTuples positions
       in do
            resultEither <- liftIO $ updatePositionsOfExercises conn exercisePositionTuples
            case resultEither of
              Right exerciseList -> redirect ("/workouts/" <> pack (show $ exerciseWorkoutId $ NE.head exerciseList) <> "/show?success=true")
              Left err -> displayPage $ errorPage err

    post "/api/update-exercise" $ do
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
          case mkExercise id title reps note position workoutId weights of
            Left err -> displayPage $ errorPage err
            Right exercise -> do
              updatedExercise <- liftIO $ updateExercise conn exercise
              either
                (displayPage . errorPage)
                (\x -> redirect ("/workouts/" <> pack (show $ exerciseWorkoutId x) <> "/show?success=true"))
                updatedExercise

    post "/api/delete-exercise" $ do
      let parseInput :: Text -> Text -> Text -> Text -> Text -> Either Text (Int, [Int], Int, [Int], Int)
          parseInput exerciseId reps position weights workoutId = (,,,,) <$> textToEitherInt exerciseId <*> textToEitherIntList reps <*> textToEitherInt position <*> textToEitherIntList weights <*> textToEitherInt workoutId
      unparsedExerciseId <- param "id"
      title <- param "title"
      unparsedReps <- param "reps"
      note <- param "note"
      unparsedPosition <- param "position"
      unparsedWeights <- param "weightsInKg"
      unparsedWorkoutId <- param "workoutId"
      case parseInput unparsedExerciseId unparsedReps unparsedPosition unparsedWeights unparsedWorkoutId of
        Left err -> displayPage $ errorPage err
        Right (exerciseId, reps, position, weights, workoutId) -> do
          case mkExercise exerciseId title reps note position workoutId weights of
            Left err -> displayPage $ errorPage err
            Right exercise -> do
              deletedExerciseEither <- liftIO $ deleteExerciseById conn exercise
              case deletedExerciseEither of
                Right deletedExercise -> redirect ("/workouts/" <> pack (show workoutId) <> "/show?success=true")
                Left err -> displayPage $ errorPage err

-- TODO: later use newtype wrapper
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
