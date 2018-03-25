module Update exposing (update, Msg(..))

import Model exposing (Note, Tag, Model, model)

type Msg =
  AddNote String |
  ChangeNewNoteTitle String

update: Msg -> Model -> Model
update msg model =
  {model |
    notes = notes msg model.notes,
    newNoteTitle = newNoteTitle msg model.newNoteTitle
  }

notes: Msg -> List Note -> List Note
notes msg state =
  case msg of
    AddNote title -> List.append state [createNote title]
    _ -> state

createNote: String -> Note
createNote title =
    Note (Tag title) [] title

newNoteTitle: Msg -> String -> String
newNoteTitle msg state =
  case msg of
    ChangeNewNoteTitle s -> s
    AddNote _ -> ""
