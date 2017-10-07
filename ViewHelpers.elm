module ViewHelpers exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Notes exposing (Note, Tag, GroupOfNotes, MultigroupOfNotes)
import List exposing (map, foldl)


tagList: List Tag -> String
tagList tags =
  tags
    |> map .name
    |> map (\t -> t ++ " ")
    |> foldl (++) ""

line {title, tags} =
  div []
    [text (title ++ "   " ++ (tagList tags))]

lines notes =
  div [style [("margin-left", "20px")]] (map line notes)

groupsView: List GroupOfNotes -> Html Never
groupsView groups =
  let
    group = \{groupingNote, group} -> div []
      [ text groupingNote.title
      , lines group
      ]
  in
    div [] (map group groups)

multigroupsView: List MultigroupOfNotes -> Html Never
multigroupsView groups =
  let
    group = \{groupingNotes, group} -> div []
      [ text (groupingNotes |> map .title |> String.join " ")
      , lines group
      ]
  in
    div [] (map group groups)
