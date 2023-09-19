{-# LANGUAGE OverloadedStrings #-}

module Webapp (mkApp) where

import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (IdentityT, runIdentityT))
import qualified Controllers.Controller as Controllers
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
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, status500)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Text.Blaze.Html (Html)
import Text.Read (readMaybe)
import Web.Scotty (ActionM, Param, Parsable (parseParam), body, defaultHandler, delete, get, html, middleware, param, params, patch, post, readEither, redirect, rescue, scottyApp, setHeader, status, text)

mkApp :: Connection -> IO Application
mkApp conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    -- logs all requests in console
    middleware logStdoutDev
    -- serves static files from the "static" directory
    middleware $ staticPolicy (addBase "static")

    -- catchall error handler. Displays errors occuring inside API controllers.
    defaultHandler Controllers.customErrorHandler

    get "/" $ Controllers.landingPage conn

    get "/workouts/:id/edit" $ Controllers.updateWorkout conn
    get "/workouts/:id/show" $ Controllers.readWorkout conn
    get "/workouts/:id/delete" $ Controllers.deleteWorkout conn
    get "/workouts/:id/exercises/order" $ Controllers.orderWorkoutExercises conn

    -- NOTE: using create update delete API routes with POST instead of
    -- PATCH/PUT or DELETE since we are using html and css only and html forms
    -- only support GET and POST verbs.
    post "/api/create-workout" $ Controllers.apiCreateWorkout conn
    post "/api/update-workout" $ Controllers.apiUpdateWorkout conn
    post "/api/delete-workout" $ Controllers.apiDeleteWorkout conn

    get "/exercises/:id/edit" $ Controllers.updateExercise conn
    get "/exercises/:id/delete" $ Controllers.deleteExercise conn

    post "/api/create-exercise" $ Controllers.apiCreateExercise conn
    -- TODO: perhaps rename to updateExercisePositions
    -- bulk updates exercises, used for updating their position/order
    post "/api/update-exercises" $ Controllers.apiUpdateExercises conn
    post "/api/update-exercise" $ Controllers.apiUpdateExercise conn
    post "/api/delete-exercise" $ Controllers.apiDeleteExercise conn
