module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Note, Tag, GroupOfNotes, MultigroupOfNotes, Model)
import Update exposing (Msg(..))
import List exposing (map, foldl)

view: Model -> Html Msg
view model =
  div []
  [ listView model.notes
  , addNoteInput model.newNoteTitle
  ]

listView: List Note -> Html Msg
listView notes =
  div [] (List.map noteView notes)

noteView: Note -> Html Msg
noteView note =
  div []
  [ text note.title
  ]

addNoteInput: String -> Html Msg
addNoteInput newNoteTitle =
  div []
    [ input [onInput ChangeNewNoteTitle, value newNoteTitle] []
    , button [onClick AddNote] [text "+"]
    ]



tagList: List Tag -> String
tagList tags =
  tags
    |> List.map .name
    |> List.map (\t -> t ++ " ")
    |> foldl (++) ""

line {title, tags} =
  div []
    [text (title ++ "   " ++ (tagList tags))]

lines notes =
  div [style [("margin-left", "20px")]] (List.map line notes)

groupsView: List GroupOfNotes -> Html Never
groupsView groups =
  let
    group = \{groupingNote, group} -> div []
      [ text groupingNote.title
      , lines group
      ]
  in
    div [] (List.map group groups)

multigroupsView: List MultigroupOfNotes -> Html Never
multigroupsView groups =
  let
    group = \{groupingNotes, group} -> div []
      [ text (groupingNotes |> List.map .title |> String.join " ")
      , lines group
      ]
  in
    div [] (List.map group groups)
