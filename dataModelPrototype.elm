import Html exposing (text)
import List exposing (..)
import Set exposing (Set)

type alias Tag =
  { category: String
  , name: String
  }

chapter = Tag "root" "chapter"
scene = Tag "root" "scene"
storyArc = Tag "root" "storyArc"
side = Tag "root" "side"
character = Tag "root" "character"
object = Tag "root" "object"
place = Tag "root" "place"
event = Tag "root" "event"
time = Tag "root" "time"

goodBad = Tag "character" "GoodBad"
badBad = Tag "character" "BadBad"
good = Tag "side" "Good"
bad = Tag "side" "Bad"
one = Tag "chapter" "1"
two = Tag "chapter" "2"
three = Tag "chapter" "3"
four = Tag "chapter" "4"

tags: List Tag
tags =
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

type Note = Note (List Tag) String
tagsInNote: Note -> List Tag
tagsInNote (Note tags _) = tags

tagsOfCategory: String -> Note -> List String
tagsOfCategory category (Note tags _) =
  map .name (filter (isA category) tags)

tagsInNotesOfCategory: String -> List Note -> List String
tagsInNotesOfCategory cat notes =
  concat (map (tagsOfCategory cat) notes)
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

isA: String -> Tag -> Bool
isA tag = .category >> (==) tag

has: List Tag -> Note -> Bool
has targetTags note =
  let
    noteTags = Set.fromList (map .name (tagsInNote note))
    tags = Set.fromList (map .name targetTags)
    intersection = Set.intersect noteTags tags
  in
    Set.size tags == Set.size intersection



chapters = filter (isA "chapter") tags
sides = filter (isA "side") tags

chaptersWithGoodBad =
  let
    rels =  (filter (has [goodBad]) notes)
  in
    tagsInNotesOfCategory "chapter" rels

sidesOfGoodBad =
  let
    rels =  (filter (has [goodBad]) notes)
  in
    tagsInNotesOfCategory "side" rels

goodBadChaptersBySide: List (List String)
goodBadChaptersBySide =
  let
    goodBadChaptersWithSide: Tag -> List Note
    goodBadChaptersWithSide = \side -> filter (has [side, goodBad]) notes
    goodBadNotesBySide: List (List Note)
    goodBadNotesBySide = map goodBadChaptersWithSide sides
    noteToChapter = tagsOfCategory "chapter"
  in
    (map (\notes -> concat (map noteToChapter notes)) goodBadNotesBySide)
  

main = text (toString goodBadChaptersBySide)
