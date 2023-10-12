{-# LANGUAGE OverloadedStrings #-}

module Controllers.Controller (readWorkout, updateWorkout, deleteWorkout, orderWorkoutExercises, apiCreateWorkout, apiUpdateWorkout, apiDeleteWorkout, updateExercise, deleteExercise, apiCreateExercise, apiUpdateExercisePositions, apiUpdateExercise, apiDeleteExercise, displayPage, customErrorHandler, showLandingPage) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers.Exercise (apiCreateExercise, apiDeleteExercise, apiUpdateExercise, apiUpdateExercisePositions, deleteExercise, updateExercise)
import Controllers.Util (displayPage, displayErrorPage)
import Controllers.Workout (apiCreateWorkout, apiDeleteWorkout, apiUpdateWorkout, deleteWorkout, orderWorkoutExercises, readWorkout, updateWorkout)
import Data.Text.Lazy (Text, split, unpack)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import qualified Database.DB as DB
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (status400)
import Views.Page
import Web.Scotty (ActionM, param, rescue, status)

customErrorHandler :: Text -> ActionM ()
customErrorHandler err = do
  status status400
  displayErrorPage err

showLandingPage :: Connection -> ActionM ()
showLandingPage conn = do
  success <- param "success" `rescue` (\_ -> return False)
  currentDate <- liftIO (utctDay <$> getCurrentTime)
  workoutsEither <- liftIO (DB.getWorkouts conn)
  either displayErrorPage (displayPage . landingPage success (mkCurrentDate currentDate)) workoutsEither
