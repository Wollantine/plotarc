module Update exposing (update, Msg(..))

import Model exposing (Note, Tag, Model, model)

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
