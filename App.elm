import Html exposing (..)
import Notes exposing (Note, Tag)
import ViewHelpers exposing (..)

main = Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

-- MODEL

type alias Model =
  { notes: List Note
  }

model: Model
model = Model [Note (Tag "Chapter") [] "Chapter"]

-- UPDATE

type Msg =
  AddNote Note

update: Msg -> Model -> Model
update msg model =
  {model | notes = notes msg model.notes}

notes: Msg -> List Note -> List Note
notes msg model =
  case msg of
    AddNote note -> note :: model

-- VIEW

view: Model -> Html Msg
view model =
  listView model.notes

listView: List Note -> Html Msg
listView notes =
  div [] (List.map noteView notes)

noteView: Note -> Html Msg
noteView note =
  div [] [
    text note.title
  ]
