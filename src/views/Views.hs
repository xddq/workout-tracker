-- gives ability to specify data of type Text with "anythinghere" instead of
-- only String
{-# LANGUAGE OverloadedStrings #-}

module Views
  ( errorPage,
    successPage,
    landingPage,
    showWorkoutPage,
    showOrderExercisesPage,
    editWorkoutPage,
    deleteExercisePage,
    deleteWorkoutPage,
    editExercisePage,
    htmlToText,
    CurrentDate,
    mkCurrentDate,
    makeHtmlHead,
  )
where

import Control.Monad (forM_)
import Data.Text.Lazy (Text, pack, split, unpack)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Database (Exercise (Exercise, exerciseId, exerciseNote, exercisePosition, exerciseReps, exerciseTitle, exerciseWeightsInKg, exerciseWorkoutId), Workout (Workout, workoutDate, workoutId, workoutType), repsToText, weightsToText)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)

newtype CurrentDate = CurrentDate Day

mkCurrentDate :: Day -> CurrentDate
mkCurrentDate = CurrentDate

instance Show CurrentDate where
  -- days may have 1 or 2 chars, then one space, then month with one or two
  -- letters then space and a 4 char year. Example: 23.07.2023
  show (CurrentDate a) = formatTime defaultTimeLocale "%-d.%-m.%Y" a

newtype Title = Title String

instance Show Title where
  show (Title x) = x

mkTitle :: String -> Title
mkTitle = Title

-- TODO/Maybe make data constructor for page and add all pages there..?
type Page = String

-- snippets

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

showWorkoutSnippet :: Workout -> Html
showWorkoutSnippet (Workout wId wType wDate wNote) = docTypeHtml $ do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Workout"
    H.div ! class_ "content" $ do
      p $ do
        strong "Datum: "
        toHtml $ show $ mkCurrentDate wDate
      p $ do
        strong "Art des Workouts: "
        toHtml wType
      p $ do
        strong "Notiz: "
        toHtml wNote

-- (CreateExerciseInput title reps note position weightInKg workoutId)

-- TODO: rename to "createX" to use same naming as in the api routes.
addExerciseSnippet :: Workout -> Html
addExerciseSnippet x = docTypeHtml $ do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Übung hinzufügen"
    H.form ! target "_self" ! action "/api/create-exercise" ! method "post" $ do
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Titel der Übung"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "title" ! required "" ! placeholder "Squats"
      input ! type_ "hidden" ! name "workoutId" ! value (toValue (workoutId x))
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Anzahl der Wiederholungen. Wiederholung pro Satz getrennt durch Komma. Z.b. 10,9,8 für 3 Sätze mit erst 10, dann 9, dann 8 Wiederholungen."
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "reps" ! required "" ! pattern "^\\d{1,2}(?:,\\d{1,2})*$" ! placeholder "8,8,8"
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Gewicht in Kilogram. Nur ganze Zahlen. Gewicht pro Satz getrennt durch Komma. Z.b. 40,35,30 für eine Übung mit 3 Sätzen und erst 40, dann 35, dann 30 kg je Satz."
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "weightsInKg" ! required "" ! pattern "^\\d{1,3}(?:,\\d{1,3})*$" ! placeholder "20,20,20"
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Notiz (z.B. Achten die volle Range of Motion zu nutzen, oder z.B. Rücken möglichst gerade halten)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "note" ! value ""
      H.div ! class_ "field" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Eintrag erstellen"

editExerciseSnippet :: Exercise -> Html
editExerciseSnippet x = docTypeHtml $ do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Übung bearbeiten"
    H.form ! target "_self" ! action "/api/update-exercise" ! method "post" $ do
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Titel der Übung"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "title" ! required "" ! value (toValue $ exerciseTitle x)
      input ! type_ "hidden" ! name "id" ! value (toValue (exerciseId x))
      input ! type_ "hidden" ! name "workoutId" ! value (toValue (exerciseWorkoutId x))
      input ! type_ "hidden" ! name "position" ! value (toValue (exercisePosition x))
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Anzahl Wiederholungen - Die Wiederholungen pro Satz sind durch ein Komma getrennt. Z.b. für 3 Sätze a 8 Wiederholungen: 8,8,8"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "reps" ! required "" ! pattern "^\\d{1,2}(?:,\\d{1,2})*$" ! value (toValue (repsToText $ exerciseReps x))
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Gewicht in Kilogram. Nur ganze Zahlen. Gewicht pro Satz getrennt durch Komma. Z.b. 40,35,30 für eine Übung mit 3 Sätzen und erst 40, dann 35, dann 30 kg je Satz."
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "weightsInKg" ! required "" ! value (toValue $ (weightsToText $ exerciseWeightsInKg x))
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Notiz (z.B. Achten die volle Range of Motion zu nutzen, oder z.B. Rücken möglichst gerade halten)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "note" ! value (toValue $ exerciseNote x)
      H.div ! class_ "field is-grouped" $ do
        H.div ! class_ "control" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Speichern"
        H.div ! class_ "control" $ a ! href (toValue $ buildHrefToWorkout x) ! class_ "button is-link" $ "Zurück"

addWorkoutSnippet :: CurrentDate -> [Workout] -> Html
addWorkoutSnippet currentDate xs = docTypeHtml $ do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Workout hinzufügen"
    H.form ! target "_self" ! action "/api/create-workout" ! method "post" $ do
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Datum (Korrektes Format ist z.b. 26.03.2023 oder 26.3.2023)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! A.id "dateInput" ! name "date" ! pattern "([1-9]|[0-2][0-9]|3[0-1])\\.(0[1-9]|[1-9]|1[0-2])\\.20[0-9]{2}" ! value (toValue $ show currentDate) ! required ""
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Art des Workouts z.b. Ganzkörper, Pull, Beine, etc."
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "type" ! required ""
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Basierend auf vorherigem Workout (Workout wird vorausgefüllt mit\n              allen Übungen des ausgewählten Eintrags)"
        H.div ! class_ "control" $ H.div ! class_ "select" $ select ! name "prefillWorkoutId" $ do
          option ! value "-1" $ "Nicht basierend auf vorherigem Workout"
          workoutOptionsListSnippet xs
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Notiz zum Workout z.b Sehr fit heute, aktuell in der Diät, ausnahmsweise abends, etc.."
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "note" ! required "" ! value (toValue $ pack " ")
      H.div ! class_ "field" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Eintrag erstellen"

workoutOptionsListSnippet :: [Workout] -> Html
workoutOptionsListSnippet workouts = forM_ workouts $ \workout ->
  H.option ! A.value (toValue $ workoutId workout) $ toHtml $ pack (show $ mkCurrentDate $ workoutDate workout) <> " - " <> workoutType workout

editWorkoutSnippet :: Workout -> Html
editWorkoutSnippet (Workout wId wType wDate wNote) = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Workout bearbeiten"
    H.form ! target "_self" ! action "/api/update-workout" ! method "post" $ do
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Datum (Korrektes Format ist z.b. 26.03.2023 oder 26.3.2023)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! A.id "dateInput" ! name "date" ! pattern "([1-9]|[0-2][0-9]|3[0-1])\\.(0[1-9]|[1-9]|1[0-2])\\.20[0-9]{2}" ! value (toValue $ show $ mkCurrentDate wDate) ! required ""
        input ! type_ "hidden" ! name "id" ! value (toValue wId)
        input ! type_ "hidden" ! name "id" ! value (toValue wNote)
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Art des Workouts z.b. Ganzkörper, Pull, Beine, etc. (Feld leer lassen, wenn die Art eines vorherigen\n              Workouts übernommen werden soll)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "type" ! value (toValue wType)
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Notiz zum Workout z.b Sehr fit heute, aktuell in der Diät, ausnahmsweise abends, etc.."
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "note" ! value (toValue wNote)
      H.div ! class_ "field" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Eintrag bearbeiten"

buildHrefToWorkout :: Exercise -> Text
buildHrefToWorkout x = "/workouts/" <> (pack $ show $ exerciseWorkoutId x) <> "/show"

deleteExerciseSnippet :: Exercise -> Html
deleteExerciseSnippet exercise = do
  H.div ! class_ "section" $ H.div ! class_ "container" $ do
    H.div ! class_ "title is-4" $ "Übung löschen"
    H.div ! class_ "card" $ do
      H.div ! class_ "card-content" $ H.div ! class_ "content" $ do
        "Bist du dir sicher, dass du die Übung mit dem Titel \""
        strong $ toHtml (exerciseTitle exercise)
        "\" unwiderruflich löschen möchtest?"
      footer ! class_ "card-footer" $ do
        H.form ! class_ "card-footer-item" ! target "_self" ! action "/" ! method "get" $ do
          a ! class_ "card-footer-item button is-primary" ! href (toValue $ buildHrefToWorkout exercise) $ "Zurück"
        H.form ! class_ "card-footer-item" ! target "_self" ! action "/api/delete-exercise" ! method "post" $ do
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "id" ! value (toValue $ show $ exerciseId exercise)
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "title" ! value (toValue $ show $ exerciseTitle exercise)
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "reps" ! value (toValue $ repsToText $ exerciseReps exercise)
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "note" ! value (toValue $ show $ exerciseNote exercise)
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "position" ! value (toValue $ show $ exercisePosition exercise)
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "weightsInKg" ! value (toValue $ weightsToText $ exerciseWeightsInKg exercise)
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "workoutId" ! value (toValue $ show $ exerciseWorkoutId exercise)
          input ! class_ "card-footer-item button is-danger" ! type_ "submit" ! value "Löschen"

deleteWorkoutSnippet :: Workout -> Html
deleteWorkoutSnippet x = do
  H.div ! class_ "section" $ H.div ! class_ "container" $ do
    H.div ! class_ "title is-4" $ "Übung löschen"
    H.div ! class_ "card" $ do
      H.div ! class_ "card-content" $ H.div ! class_ "content" $ do
        "Bist du dir sicher, dass du das Workout vom " <> toHtml (show $ mkCurrentDate $ workoutDate x) <> " der Art \""
        strong $ toHtml (workoutType x)
        "\" unwiderruflich löschen möchtest?"
      footer ! class_ "card-footer" $ do
        H.form ! class_ "card-footer-item" ! target "_self" ! action "/" ! method "get" $ do
          a ! class_ "card-footer-item button is-primary" ! href "/" $ "Zurück"
        H.form ! class_ "card-footer-item" ! target "_self" ! action "/api/delete-workout" ! method "post" $ do
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "workoutId" ! value (toValue $ show $ workoutId x)
          input ! class_ "card-footer-item button is-danger" ! type_ "submit" ! value "Löschen"

displayWorkoutListSnippet :: CurrentDate -> [Workout] -> Html
displayWorkoutListSnippet x ys = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Liste aller Workouts"
    table ! class_ "table" $ do
      thead $ tr $ do
        th "Art des Workouts"
        th "Datum des Workouts"
        th "Notiz"
        th "Eintrag ansehen"
        th "Eintrag bearbeiten"
        th "Eintrag löschen"
      tbody $ mapM_ displayWorkoutListItemSnippet ys

displayWorkoutListItemSnippet :: Workout -> Html
displayWorkoutListItemSnippet (Workout wId wType wDate wNote) = do
  H.tr $ do
    H.td $ toHtml wType
    H.td $ toHtml $ show $ mkCurrentDate wDate
    H.td $ toHtml wNote
    H.td $ a ! href (toValue $ "/workouts/" ++ show wId ++ "/show") ! class_ "button is-primary" $ "Ansehen"
    H.td $ a ! href (toValue $ "/workouts/" ++ show wId ++ "/edit") ! class_ "button is-primary" $ "Bearbeiten"
    H.td $ a ! href (toValue $ "/workouts/" ++ show wId ++ "/delete") ! class_ "button is-danger" $ "Löschen"

displayExerciseListSnippet :: [Exercise] -> Html
displayExerciseListSnippet xs = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Liste aller Übungen"
    H.div ! class_ "table-container" $ do
      table ! class_ "table" $ do
        thead $ tr $ do
          th $ abbr ! A.title "Position" $ "Pos"
          th $ abbr ! A.title "Name der Übung" $ "Übung"
          th $ abbr ! A.title "Gewicht pro Wiederholung" $ "Gewicht"
          th $ abbr ! A.title "Wiederholungen pro Satz" $ "Reps"
          th "Notiz"
          th "Bearbeiten"
          th "Löschen"
        tbody $ mapM_ displayExerciseListItemSnippet xs
    if null xs then mempty else H.div $ a ! href (toValue $ "/workouts/" ++ show (exerciseWorkoutId $ Prelude.head xs) ++ "/exercises/order") ! class_ "button is-primary" $ "Reihenfolge anpassen"

displayExerciseListItemSnippet :: Exercise -> Html
displayExerciseListItemSnippet (Exercise id title reps note position workoutId weights) = do
  H.tr $ do
    H.td $ toHtml position
    H.td $ toHtml title
    -- TODO: why not PGArray Int here..?
    H.td $ toHtml $ weightsToText weights
    H.td $ toHtml $ repsToText reps
    H.td $ toHtml note
    H.td $ a ! href (toValue $ "/exercises/" ++ show id ++ "/edit") ! class_ "button is-primary" $ "Bearbeiten"
    H.td $ a ! href (toValue $ "/exercises/" ++ show id ++ "/delete") ! class_ "button is-danger" $ "Löschen"

displayOrderExerciseListSnippet :: [Exercise] -> Html
displayOrderExerciseListSnippet xs =
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Liste aller Übungen"
    H.div ! class_ "block" $ do
      "Um die Reihenfolge der Übungen anzupassen muss man die Werte der Spalte "
      strong "Pos"
      " (Position) anpassen. Es darf keine Position doppelt vergeben werden. Positionen werden der Reihe nach (von klein nach groß) angezeigt. Also zuerst die Übung mit Position 1, dann die Übung mit Position 2, etc."
    H.form ! target "_self" ! action "/api/update-exercises" ! method "post" $ do
      H.div ! class_ "table-container" $ do
        table ! class_ "table" $ do
          thead $ tr $ do
            th $ abbr ! A.title "Position" $ "Pos"
            th $ abbr ! A.title "Name der Übung" $ "Übung"
            th $ abbr ! A.title "Wiederholungen pro Satz" $ "Reps"
          tbody $ mapM_ displayOrderExerciseListItemSnippet xs
      H.div ! class_ "field" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Reihenfolge speichern"

displayOrderExerciseListItemSnippet :: Exercise -> Html
displayOrderExerciseListItemSnippet (Exercise id title reps note position weightInKg workoutId) = do
  H.tr $ do
    H.td $ H.div ! class_ "control" $ input ! class_ "input" ! type_ "number" ! name "position[]" ! required "" ! value (toValue position) ! A.min "1"
    H.td $ toHtml title
    H.td $ toHtml $ repsToText reps
  input ! type_ "hidden" ! name "exerciseId[]" ! value (toValue id)

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

-- __snippets__

-- pages
landingPage :: Success -> CurrentDate -> Either Text [Workout] -> Html
landingPage s date (Right workouts) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Workout Tracker"
  body $ do
    successSnippet s
    addWorkoutSnippet date workouts
    displayWorkoutListSnippet date workouts
landingPage _ _ (Left err) = errorPage err

-- TODO: Perhaps use/try 'Maybe a' newtype wrapper with semantics of -> if it
-- has a value we display the successsnippet with a message
type Success = Bool

showWorkoutPage :: Success -> Workout -> Either Text [Exercise] -> Html
showWorkoutPage s workout (Right exercises) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Show Workout"
  body $ do
    successSnippet s
    showWorkoutSnippet workout
    displayExerciseListSnippet exercises
    backToHomePageSnippet
    addExerciseSnippet workout
showWorkoutPage _ _ (Left err) = errorPage err

showOrderExercisesPage :: Either Text [Exercise] -> Html
showOrderExercisesPage (Right xs) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Order Exercises"
  body $ do
    backToHomePageSnippet
    displayOrderExerciseListSnippet xs
showOrderExercisesPage (Left err) = errorPage err

-- TODO: Just an idea for now. Maybe we could create generic/polymorphic edit
-- and delete pages later (after done with the app).
editWorkoutPage :: Either Text Workout -> Html
editWorkoutPage (Right workout) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Edit Workout"
  body $ editWorkoutSnippet workout
editWorkoutPage (Left err) = errorPage err

editExercisePage :: Either Text Exercise -> Html
editExercisePage (Right exercise) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Edit Exercise"
  body $ editExerciseSnippet exercise
editExercisePage (Left err) = errorPage err

deleteExercisePage :: Either Text Exercise -> Html
deleteExercisePage (Right exercise) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Delete Exercise"
  body $ deleteExerciseSnippet exercise
deleteExercisePage (Left err) = errorPage err

-- deleteWorkoutPage :: Workout -> Html
-- deleteWorkoutPage x = docTypeHtml $ do
--   makeHtmlHead $ mkTitle "Delete Workout"
--   body $ deleteWorkoutSnippet x

deleteWorkoutPage :: Either Text Workout -> Html
deleteWorkoutPage (Right workout) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Delete Workout"
  body $ deleteWorkoutSnippet workout
deleteWorkoutPage (Left err) = errorPage err

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

-- __pages__

-- TODO: perhaps either use maybe or throw exception on 0. should never be 0.
charactersAfterDot :: String -> Int
charactersAfterDot str =
  case dropWhile (/= '.') str of
    [] -> 0 -- If there is no dot, return 0.
    dotStr -> length (tail dotStr) -- Get the length of characters after the dot.

htmlToText :: Html -> Text
htmlToText = renderHtml
