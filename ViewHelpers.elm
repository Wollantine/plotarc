module ViewHelpers exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Notes exposing (Note, Tag, GroupOfNotes)
import List exposing (map, foldl)

view: List Note -> Html Never
view notes =
  div [] (List.map noteView notes)

noteView: Note -> Html Never
noteView note =
  div [] [
    text note.title
  ]

tagList: List Tag -> String
tagList tags =
  tags
    |> map .name
    |> map (\t -> t ++ " ")
    |> foldl (++) ""

groupsView: List GroupOfNotes -> Html Never
-- groupsView: List (String, List (String, String)) -> Html Never
groupsView groups =
  let
    line = \{title, tags} -> div []
      [text (title ++ "   " ++ (tagList tags))]
    group = \{groupTitle, groupNotes} -> div []
      [ text groupTitle.title
      , div [style [("margin-left", "20px")]] (map line groupNotes)
      ]
  in
    div [] (map group groups)
