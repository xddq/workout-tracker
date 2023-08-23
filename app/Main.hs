{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Webapp (mkApp)

main :: IO ()
main = do
  -- loads .env into environment variables
  loadFile defaultConfig
  host <- getEnv "DB_HOST"
  conn <- connect defaultConnectInfo {connectHost = host, connectDatabase = "todo-app", connectUser = "psql", connectPassword = "psql"}
  waiApp <- mkApp conn
  putStrLn "Running app on localhost:3000"
  run 3000 waiApp
