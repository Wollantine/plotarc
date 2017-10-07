import Html exposing (..)
import ViewHelpers exposing (view)
import Update exposing (update)
import Notes exposing (model)

main = Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }
