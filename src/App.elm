import Html exposing (..)
import View exposing (view)
import Update exposing (update)
import Model exposing (model)

main = Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }
