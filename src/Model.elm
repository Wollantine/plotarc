module Model exposing (..)


type alias Tag = {name: String}

type alias Note =
  { tag: Tag
  , tags: List Tag
  , title: String
  }

note: String -> List String -> String -> Note
note tag tags title =
  Note (Tag tag) (List.map (\t -> Tag t) tags) title

type alias GroupOfNotes =
  { groupingNote: Note
  , group: List Note
  }

type alias MultigroupOfNotes =
  { groupingNotes: List Note
  , group: List Note
  }

chapter = Tag "chapter"
scene = Tag "scene"
storyArc = Tag "storyArc"
side = Tag "side"
character = Tag "character"
object = Tag "object"
place = Tag "place"
event = Tag "event"
time = Tag "time"

goodBad = Tag "GoodBad"
badBad = Tag "BadBad"
good = Tag "Good"
bad = Tag "Bad"
one = Tag "1"
two = Tag "2"
three = Tag "3"
four = Tag "4"
ringOfPower = Tag "Ring of Power"

{- Provisional -}
leafTag = Tag "leaf"

notes: List Note
notes =
  [ Note chapter [] "Chapter"
  , Note scene [] "Scene"
  , Note character [] "Character"
  , Note one [chapter] "1"
  , Note two [chapter, goodBad] "2"
  , Note three [chapter] "3"
  , Note four [chapter] "4"
  , Note good [side] "Good"
  , Note bad [side] "Bad"
  , Note goodBad [character] "GoodBad"
  , Note badBad [character] "BadBad"
  , Note leafTag [goodBad, good, one, scene] "GoodBad has doubts"
  , Note leafTag [goodBad, good, two, scene] "GoodBad discovers he has been betrayed"
  , Note leafTag [goodBad, bad, three, scene] "GoodBad turns mad and kills everyone"
  , Note leafTag [goodBad, bad, four, scene] "GoodBad calms down and rethinks his purpose"
  , Note leafTag [goodBad, object] "GoodBad's wristwatch"
  , Note leafTag [scene, two] "GoodBad turns bad"
  , Note leafTag [badBad, ringOfPower, one, scene] "BadBad steals the Ring of Power"
  , Note leafTag [badBad, ringOfPower, four, scene] "BadBad breaks the Ring of Power and becomes a god"
  ]


-- MODEL

type alias Model =
  { notes: List Note
  , newNoteTitle: String
  }

model: Model
model = Model notes ""
