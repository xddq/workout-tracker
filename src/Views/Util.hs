{-# LANGUAGE OverloadedStrings #-}

module Views.Util where

import Data.Text.Lazy (Text)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- simple newtype wrapper around day to implement custom Show typeclass
newtype CurrentDate = CurrentDate Day

mkCurrentDate :: Day -> CurrentDate
mkCurrentDate = CurrentDate

-- days may have 1 or 2 chars, then one space, then month with one or two
-- letters then space and a 4 char year. Example: 23.07.2023
instance Show CurrentDate where
  show (CurrentDate a) = formatTime defaultTimeLocale "%-d.%-m.%Y" a

-- TODO: does this need to be a newtype wrapper..?
newtype Title = Title String

instance Show Title where
  show (Title x) = x

mkTitle :: String -> Title
mkTitle = Title

-- TODO: Perhaps use/try 'Maybe a' newtype wrapper with semantics of -> if it
-- has a value we display the successsnippet with a message
type Success = Bool

-- TODO/Maybe make data constructor for page and add all pages there..?
type Page = String

{-
 - Used to creat the html head for each page. Adds required metadata, loads
 - bulma css and sets the page title.
 -}
makeHtmlHead :: Title -> Html
makeHtmlHead x =
  H.head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title $ toHtml $ show x
    link ! rel "stylesheet" ! href "/bulma-0.9.4.min.css"

redirectToHomeSnippet :: Html
redirectToHomeSnippet = p $ do
  "Klicke"
  a ! href "/" $ "hier"
  "um zurück zur Hauptseite zu gelangen."

successSnippet :: Success -> Html
successSnippet True = section ! class_ "section" $ H.div ! class_ "container" $ H.div ! class_ "notification is-success subtitle is-5" $ "Aktion wurde erfolgreich ausgeführt!"
successSnippet False = mempty

backToHomePageSnippet :: Html
backToHomePageSnippet = section ! class_ "section" $ H.div ! class_ "container" $ a ! href "/" ! class_ "button is-link" $ "Zurück zur Startseite"

successPage :: Html
successPage = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Success"
  body $ section ! class_ "section" $ H.div ! class_ "container" $ H.div ! class_ "notification is-success subtitle is-5" $ do
    "Erfolgreich durgeführt! Klicke"
    a ! href "/" $ " hier "
    "um zurück zur Startseite zu gelangen."

errorPage :: Text -> Html
errorPage err = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Error"
  body $ section ! class_ "section" $ H.div ! class_ "container" $ H.div ! class_ "notification is-danger subtitle is-5" $ do
    "Fehler beim ausführen! Klicke"
    a ! href "/" $ " hier "
    toHtml ("um zurück zur Startseite zu gelangen. Fehler: " <> err)

-- TODO: perhaps either use maybe or throw exception on 0. should never be 0.
charactersAfterDot :: String -> Int
charactersAfterDot str =
  case dropWhile (/= '.') str of
    [] -> 0 -- If there is no dot, return 0.
    dotStr -> length (tail dotStr) -- Get the length of characters after the dot.

repsToText :: [Int] -> String
repsToText = foldl (\acc curr -> if null acc then show curr else acc ++ "," ++ show curr) ""

weightsToText :: [Int] -> String
weightsToText = repsToText
