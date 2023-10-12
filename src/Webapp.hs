{-# LANGUAGE OverloadedStrings #-}

module Webapp (mkApp) where

import qualified Controllers.Controller as Controllers
import Database.PostgreSQL.Simple (Connection)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty (ActionM, Param, Parsable (parseParam), body, defaultHandler, delete, get, html, middleware, param, params, patch, post, readEither, redirect, rescue, scottyApp, setHeader, status, text)

mkApp :: Connection -> IO Application
mkApp conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    -- logs all requests in console
    middleware logStdoutDev
    -- serves static files from the "static" directory
    middleware $ staticPolicy (addBase "static")
    -- catchall error handler
    defaultHandler Controllers.customErrorHandler

    get "/" $ Controllers.showLandingPage conn

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
    post "/api/update-exercise" $ Controllers.apiUpdateExercise conn
    post "/api/update-exercise-positions" $ Controllers.apiUpdateExercisePositions conn
    post "/api/delete-exercise" $ Controllers.apiDeleteExercise conn
