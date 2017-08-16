import Html exposing (text)
import List exposing (..)
import Set exposing (Set)

type alias Dimension =
  { category: String
  , name: String
  }

chapter = Dimension "root" "chapter"
scene = Dimension "root" "scene"
storyArc = Dimension "root" "storyArc"
side = Dimension "root" "side"
character = Dimension "root" "character"
object = Dimension "root" "object"
place = Dimension "root" "place"
event = Dimension "root" "event"
time = Dimension "root" "time"

goodBad = Dimension "character" "GoodBad"
badBad = Dimension "character" "BadBad"
good = Dimension "side" "Good"
bad = Dimension "side" "Bad"
one = Dimension "chapter" "1"
two = Dimension "chapter" "2"
three = Dimension "chapter" "3"
four = Dimension "chapter" "4"

dimensions: List Dimension
dimensions =
  [ chapter
  , scene
  , storyArc
  , side
  , character
  , object
  , place
  , event
  , time
  , goodBad
  , badBad
  , good
  , bad
  , one
  , two
  , three
  , four
  ]

type Note = Note (List Dimension) String
noteDimensions: Note -> List Dimension
noteDimensions (Note dims _) = dims

findDimensions: String -> Note -> List String
findDimensions category (Note dims _) =
  map .name (filter (isA category) dims)
  |> Set.fromList
  |> Set.toList

notes: List Note
notes =
  [ Note [goodBad, good, one] ""
  , Note [goodBad, good, two] ""
  , Note [goodBad, bad, three] ""
  , Note [goodBad, bad, four] ""
  , Note [scene, one] "GoodBad turns bad"
  ]

isA: String -> Dimension -> Bool
isA dimension = .category >> (==) dimension

has: List Dimension -> Note -> Bool
has dimensions note =
  let
    noteDims = Set.fromList (map .name (noteDimensions note))
    dims = Set.fromList (map .name dimensions)
    intersection = Set.intersect noteDims dims
  in
    Set.size dims == Set.size intersection




chapters = filter (isA "chapter") dimensions
sides = filter (isA "side") dimensions

chaptersWithGoodBad =
  let
    rels =  (filter (has [goodBad]) notes)
  in
    concat (map (findDimensions "chapter") rels)

sidesOfGoodBad =
  let
    rels =  (filter (has [goodBad]) notes)
  in
    concat (map (findDimensions "side") rels)

main = text (toString sidesOfGoodBad)
