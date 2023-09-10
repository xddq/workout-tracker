{-# LANGUAGE OverloadedStrings #-}

module Webapp (mkApp) where

import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (IdentityT, runIdentityT))
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), ToJSON (toJSON), Value, decode, encode, fromJSON, object, withObject, (.:), (.=))
import Data.Either (fromLeft, isLeft)
import Data.List (sortOn)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text, null, pack, split, unpack)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read (decimal)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Database (CreateExerciseInput (CreateExerciseInput), CreateWorkoutInput (CreateWorkoutInput), Exercise (Exercise, exerciseWorkoutId), Workout (Workout, workoutId), createExercise, createWorkout, deleteExerciseById, deleteWorkoutWithExercises, getExerciseById, getExercisesForWorkout, getHighestPositionByWorkoutId, getWorkoutById, getWorkouts, maybeToRight, updateExercise, updatePositionOfExercise, updatePositionsOfExercises, updateWorkout)
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
import Views (deleteExercisePage, deleteWorkoutPage, editExercisePage, editWorkoutPage, errorPage, htmlToText, landingPage, mkCurrentDate, showOrderExercisesPage, showWorkoutPage, successPage)
import Web.Scotty (ActionM, Param, Parsable (parseParam), body, delete, get, html, middleware, param, params, patch, post, redirect, rescue, scottyApp, setHeader, status, text)

-- days may have 1 or 2 chars, then one space, then month with one or two
-- letters then space and a 4 char year. Example: 23 07 2023
dateFormat :: String
dateFormat = "%-d.%-m.%Y"

textToDate :: String -> Maybe Day
textToDate = parseTimeM True defaultTimeLocale dateFormat

euroToCent :: Double -> Int
euroToCent x = round $ x * 100

displayPage :: Html -> ActionM ()
displayPage x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  text $ htmlToText x

-- Creates the resulting html we are going to serve based on a calculation that
-- could have failed. The first function of the tuple will be run on success and
-- the second one on failure. Recommended way to serve html in this app.
createHtml :: Either Text a -> (a -> Html, Text -> Html) -> Html
createHtml (Left err) (_, g) = g err
createHtml (Right a) (f, _) = f a

textToEitherInt :: T.Text -> Either T.Text Int
textToEitherInt txt =
  case decimal txt of
    Right (i, rest) -> if T.null rest then Right i else Left $ "Extra characters after integer: " <> rest
    _ -> Left $ "Could not parse: " <> txt

textToEitherInt2 :: T.Text -> IdentityT (Either Text) Int
textToEitherInt2 txt =
  case decimal txt of
    Right (i, rest) -> if T.null rest then IdentityT $ Right i else IdentityT $ Left $ "Extra characters after integer: " <> rest
    _ -> IdentityT $ Left $ "Could not parse: " <> txt

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
          displayPage $ createHtml workoutEither (editWorkoutPage, errorPage)

    get "/workouts/:id/show" $ do
      unparsedId <- param "id"
      success <- param "success" `rescue` (\_ -> return False)
      -- TODO: Try to figure out how to use monad transformer here. It feels
      -- like it should be possible to use something like
      -- ```
      -- exercisesEither <- runSomeTransformer $ do
      -- -- here when we get a 'Left' it should 'short
      -- -- circuit' and return the Left as result of the do
      -- -- block.
      -- Right parsedWorkoutId <- textToEitherInt
      -- -- here when we get a 'Left' it should 'short
      -- -- circuit' and return the Left as result of the do
      -- -- block.
      -- Right workout <- liftIO (getWorkoutById conn parsedWorkoutId)
      -- -- here when we get a 'Left' it should 'short
      -- -- circuit' and return the Left as result of the do
      -- -- block.
      -- Right exercises <- liftIO (getExercisesForWorkout conn (workoutId workout))
      -- return exercises
      --
      -- displayPage $ showWorkoutPage success workout exercisesEither
      -- ```
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
      unparsedId <- param "id"
      case textToEitherInt unparsedId of
        Left err -> displayPage $ errorPage err
        Right parsedWorkoutId -> do
          workoutEither <- liftIO (getWorkoutById conn parsedWorkoutId)
          displayPage $ deleteWorkoutPage workoutEither

    get "/workouts/:id/exercises/order" $ do
      workoutId <- param "id" :: ActionM Int
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
          displayPage $ deleteExercisePage exerciseEither

    post "/api/create-workout" $ do
      workoutType <- param "type"
      unparsedWorkoutId <- param "prefillWorkoutId"
      workoutDate <- param "date" :: ActionM String
      case textToDate workoutDate of
        Nothing -> displayPage $ errorPage "could not parse the given date"
        Just date -> do
          case textToEitherInt unparsedWorkoutId of
            Right parsedWorkoutId -> do
              createdWorkout <- liftIO $ createWorkout conn (CreateWorkoutInput (if T.null workoutType then "Keine Angabe" else workoutType) date parsedWorkoutId)
              if isJust createdWorkout then redirect ("/" <> "?success=true") else displayPage $ errorPage "error creating workout"
            Left err -> displayPage $ errorPage err

    post "/api/create-exercise" $ do
      title <- param "title"
      unparsedReps <- param "reps"
      note <- param "note"
      unparsedWeights <- param "weightsInKg"
      unparsedWorkoutId <- param "workoutId"
      -- TODO: maybe create "parseCreateExerciseParams" which returns either
      -- with parser errors (reps, weightsInKg, workoutId)
      -- TODO: Can we use monad transformers here to pattern match on Right (or
      -- Just) and on no match return the error..?! If so, how?
      case parseReps unparsedReps of
        Nothing -> displayPage $ errorPage "error parsing the reps"
        Just reps -> do
          case parseWeights unparsedWeights of
            Nothing -> displayPage $ errorPage "error parsing the weights"
            Just weights -> do
              case textToEitherInt unparsedWorkoutId of
                Left err -> displayPage $ errorPage err
                Right parsedWorkoutId -> do
                  position <- liftIO $ getHighestPositionByWorkoutId conn parsedWorkoutId
                  case position of
                    Left err -> displayPage $ errorPage err
                    Right position -> do
                      createdExercise <- liftIO $ createExercise conn (CreateExerciseInput title (PGArray reps) note position parsedWorkoutId (PGArray weights))
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
            result <- liftIO $ updatePositionsOfExercises conn exercisePositionTuples
            case result of
              Just _ -> do
                -- NOTE: could just pass the 'workoutId' via path or also post
                -- param. For now just query for the workoutId of any given
                -- exercise (all belong to the same exercise).
                exerciseEither <- liftIO $ getExerciseById conn $ snd $ head exercisePositionTuples
                case exerciseEither of
                  Right exercise -> redirect ("/workouts/" <> pack (show $ exerciseWorkoutId exercise) <> "/show?success=true")
                  Left err -> displayPage $ errorPage err
              Nothing -> displayPage $ errorPage "error updating the exercises"

    post "/api/update-exercise" $ do
      id <- param "id" :: ActionM Int
      title <- param "title" :: ActionM Text
      unparsedReps <- param "reps" :: ActionM Text
      note <- param "note" :: ActionM Text
      position <- param "position" :: ActionM Int
      unparsedWeights <- param "weightsInKg" :: ActionM Text
      workoutId <- param "workoutId" :: ActionM Int
      case parseReps unparsedReps of
        Just reps -> do
          case parseWeights unparsedWeights of
            Nothing -> displayPage $ errorPage "could not parse the given weights"
            Just weights -> do
              updatedExercise <- liftIO $ updateExercise conn (Exercise id title (PGArray reps) note position workoutId (PGArray weights))
              either
                (\err -> displayPage $ errorPage $ "error creating exercise. error: " <> err)
                (\x -> redirect ("/workouts/" <> pack (show $ exerciseWorkoutId x) <> "/show?success=true"))
                updatedExercise
        Nothing -> displayPage $ errorPage "could not parse the given reps"

    post "/api/delete-exercise" $ do
      id <- param "id" :: ActionM Int
      title <- param "title" :: ActionM Text
      unparsedReps <- param "reps" :: ActionM Text
      note <- param "note" :: ActionM Text
      position <- param "position" :: ActionM Int
      unparsedWeights <- param "weightsInKg" :: ActionM Text
      workoutId <- param "workoutId" :: ActionM Int
      case parseReps unparsedReps of
        Nothing -> displayPage $ errorPage "could not parse the given reps"
        Just reps -> do
          case parseWeights unparsedWeights of
            Nothing -> displayPage $ errorPage "could not parse the given weights"
            Just weights -> do
              deletedExercise <- liftIO $ deleteExerciseById conn (Exercise id title (PGArray reps) note position workoutId (PGArray weights))
              case deletedExercise of
                Just x -> redirect ("/workouts/" <> pack (show workoutId) <> "/show?success=true")
                Nothing -> displayPage $ errorPage "error deleting exercise"

    post "/api/update-workout" $ do
      workoutId <- param "id" :: ActionM Int
      workoutType <- param "type" :: ActionM Text
      workoutDate <- param "date" :: ActionM String
      case textToDate workoutDate of
        Nothing -> displayPage $ errorPage "could not parse the given date"
        Just date -> do
          _updatedItem <- liftIO $ updateWorkout conn (Workout workoutId workoutType date)
          displayPage successPage

    post "/api/delete-workout" $ do
      id <- param "workoutId" :: ActionM Int
      deletedRowsCount <- liftIO $ deleteWorkoutWithExercises conn id
      either
        (displayPage . errorPage)
        (\_ -> redirect ("/" <> "?success=true"))
        deletedRowsCount

parseReps :: Text -> Maybe [Int]
parseReps x =
  mapM textToInt $ split (== ',') x
  where
    textToInt x = readMaybe (unpack x) :: Maybe Int

parseWeights :: Text -> Maybe [Int]
parseWeights = parseReps

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
