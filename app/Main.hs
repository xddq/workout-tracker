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
  let webserverPort = 3000 :: Int
  putStrLn $ "Running app on localhost:" ++ show webserverPort
  run webserverPort waiApp
