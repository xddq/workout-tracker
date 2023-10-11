{-# LANGUAGE OverloadedStrings #-}

module Controllers.Util where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text.Lazy (Text, split, unpack)
import qualified Data.Text.Lazy as T
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Network.HTTP.Types (status400)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Views.Page (errorPage)
import Web.Scotty (ActionM, defaultHandler, param, raise, readEither, redirect, rescue, setHeader, status, text)

htmlToText :: Html -> Text
htmlToText = renderHtml

textToEitherInt :: Text -> Either Text Int
textToEitherInt = readEither

textToEitherIntList :: Text -> Either Text [Int]
textToEitherIntList txt = mapM readEither $ textToList txt
  where
    textToList :: Text -> [Text]
    textToList = split (== ',')
    readEitherToInt :: Text -> Either Text Int
    readEitherToInt = readEither

leftToErrorPage :: ExceptT Text ActionM () -> ActionM ()
leftToErrorPage e = runExceptT e >>= either (displayPage . errorPage) pure 

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
