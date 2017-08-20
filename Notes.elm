module Notes exposing (..)


type alias Tag = {name: String}

type alias Note =
  { tag: Tag
  , tags: List Tag
  , title: String
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

{- Provisional -}
leafTag = Tag "leaf"

notes: List Note
notes =
  [ Note one [chapter] "1"
  , Note two [chapter, goodBad] "2"
  , Note three [chapter] "3"
  , Note four [chapter] "4"
  , Note good [side] "Good"
  , Note bad [side] "Bad"
  , Note goodBad [character] "GoodBad"
  , Note badBad [character] "BadBad"
  , Note leafTag [goodBad, good, one] "GoodBad has doubts"
  , Note leafTag [goodBad, good, two] "GoodBad discovers he has been betrayed"
  , Note leafTag [goodBad, bad, three] "GoodBad turns mad and kills everyone"
  , Note leafTag [goodBad, bad, four] "GoodBad calms down and rethinks his purpose"
  , Note leafTag [goodBad, object] "GoodBad's wristwatch"
  , Note leafTag [scene, two] "GoodBad turns bad"
  ]
