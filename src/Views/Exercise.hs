{-# LANGUAGE OverloadedStrings #-}

module Views.Exercise (editExercisePage, showOrderExercisesPage, deleteExercisePage, addExerciseSnippet, displayExerciseListSnippet) where

import Data.Text.Lazy (Text, pack)
import Database.DB (Exercise, Workout, exerciseId, exerciseNote, exercisePosition, exerciseReps, exerciseTitle, exerciseWeightsInKg, exerciseWorkoutId, repsToText, weightsToText, workoutId)
import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Views.Util (backToHomePageSnippet, errorPage, makeHtmlHead, mkTitle)

showOrderExercisesPage :: Either Text [Exercise] -> Html
showOrderExercisesPage (Right xs) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Order Exercises"
  body $ do
    backToHomePageSnippet
    displayOrderExerciseListSnippet xs
showOrderExercisesPage (Left err) = errorPage err

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
displayExerciseListItemSnippet exercise = do
  H.tr $ do
    H.td $ toHtml $ exercisePosition exercise
    H.td $ toHtml $ exerciseTitle exercise
    -- TODO: why not PGArray Int here..?
    H.td $ toHtml $ weightsToText $ exerciseWeightsInKg exercise
    H.td $ toHtml $ repsToText $ exerciseReps exercise
    H.td $ toHtml $ exerciseNote exercise
    H.td $ a ! href (toValue $ "/exercises/" ++ show (exerciseId exercise) ++ "/edit") ! class_ "button is-primary" $ "Bearbeiten"
    H.td $ a ! href (toValue $ "/exercises/" ++ show (exerciseId exercise) ++ "/delete") ! class_ "button is-danger" $ "Löschen"

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
displayOrderExerciseListItemSnippet exercise = do
  H.tr $ do
    H.td $ H.div ! class_ "control" $ input ! class_ "input" ! type_ "number" ! name "position[]" ! required "" ! value (toValue $ exercisePosition exercise) ! A.min "1"
    H.td $ toHtml $ exerciseTitle exercise
    H.td $ toHtml $ repsToText $ exerciseReps exercise
  input ! type_ "hidden" ! name "exerciseId[]" ! value (toValue $ exerciseId exercise)
