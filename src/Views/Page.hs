-- This module re-exports functions from the underlying modules.
-- Providing/exporting only the functions the webapp will need.

module Views.Page
  ( landingPage,
    successPage,
    errorPage,
    showWorkoutPage,
    editWorkoutPage,
    deleteWorkoutPage,
    showOrderExercisesPage,
    editExercisePage,
    deleteExercisePage,
    htmlToText,
    mkCurrentDate,
  )
where

import Views.Exercise (deleteExercisePage, editExercisePage, showOrderExercisesPage)
import Views.Util (CurrentDate, Success, Title, errorPage, htmlToText, makeHtmlHead, mkCurrentDate, mkTitle, successPage, successSnippet)
import Views.Workout (addWorkoutSnippet, deleteWorkoutPage, displayWorkoutListSnippet, editWorkoutPage, landingPage, showWorkoutPage)
