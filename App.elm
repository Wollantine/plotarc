import Html exposing (..)
import Notes exposing (Note, Tag)
import ViewHelpers exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

-- MODEL

type alias Model =
  { notes: List Note
  , newNoteTitle: String
  }

model: Model
model = Model [Note (Tag "Chapter") [] "Chapter"] ""

-- UPDATE

type Msg = AddNote | ChangeNewNoteTitle String


update: Msg -> Model -> Model
update msg model =
  {model |
    notes = notes msg model.notes model,
    newNoteTitle = newNoteTitle msg model.newNoteTitle
  }

notes: Msg -> List Note -> Model -> List Note
notes msg state model =
  case msg of
    AddNote -> List.append state [createNote model]
    _ -> state

createNote: Model -> Note
createNote model =
  let
    newNoteTitle = model.newNoteTitle
  in
    Note (Tag newNoteTitle) [] newNoteTitle

newNoteTitle: Msg -> String -> String
newNoteTitle msg state =
  case msg of
    ChangeNewNoteTitle s -> s
    AddNote -> ""

-- VIEW

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
