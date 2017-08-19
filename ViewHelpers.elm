module ViewHelpers exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Notes exposing (Note)
import List exposing (map)

view: List Note -> Html Never
view notes =
  div [] (List.map noteView notes)

noteView: Note -> Html Never
noteView note =
  div [] [
    text note.title
  ]

groupsView: List (String, List (String, String)) -> Html Never
groupsView groups =
  let
    line = \(title, description) -> div []
      [text (title ++ "   " ++ description)]
    group = \(title, g) -> div []
      [ text title
      , div [style [("margin-left", "20px")]] (map line g)
      ]
  in
    div [] (map group groups)
