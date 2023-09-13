{-# LANGUAGE OverloadedStrings #-}

module Views.Workout (landingPage, showWorkoutPage, deleteWorkoutPage, editWorkoutPage, addWorkoutSnippet, displayWorkoutListSnippet) where

import Control.Monad (forM_)
import Data.Text.Lazy (Text, pack)
import Database.DB (Exercise, Workout (Workout, workoutDate, workoutId, workoutType))
import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Views.Exercise (addExerciseSnippet, displayExerciseListSnippet)
import Views.Util (CurrentDate, Success, backToHomePageSnippet, errorPage, makeHtmlHead, mkCurrentDate, mkTitle, successSnippet)

landingPage :: Success -> CurrentDate -> Either Text [Workout] -> Html
landingPage s date (Right workouts) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Workout Tracker"
  body $ do
    successSnippet s
    addWorkoutSnippet date workouts
    displayWorkoutListSnippet date workouts
landingPage _ _ (Left err) = errorPage err

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

deleteWorkoutPage :: Either Text Workout -> Html
deleteWorkoutPage (Right workout) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Delete Workout"
  body $ deleteWorkoutSnippet workout
deleteWorkoutPage (Left err) = errorPage err

editWorkoutPage :: Either Text Workout -> Html
editWorkoutPage (Right workout) = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Edit Workout"
  body $ editWorkoutSnippet workout
editWorkoutPage (Left err) = errorPage err

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
