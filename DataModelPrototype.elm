module DataModelPrototype exposing (..)

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

type alias Note =
  { tags: List Tag
  , title: String
  }

tagsInNote: Note -> List Tag
tagsInNote {tags} = tags

tagsOfCategory: String -> Note -> List String
tagsOfCategory category {tags} =
  map .name (filter (isA category) tags)

tagsInNotesOfCategory: String -> List Note -> List String
tagsInNotesOfCategory cat notes =
  concat (map (tagsOfCategory cat) notes)
  |> Set.fromList
  |> Set.toList

notes: List Note
notes =
  [ Note [goodBad, good, one] "GoodBad has doubts"
  , Note [goodBad, good, two] "GoodBad discovers he has been betrayed"
  , Note [goodBad, bad, three] "GoodBad turns mad and kills everyone"
  , Note [goodBad, bad, four] "GoodBad calms down and rethinks his purpose"
  , Note [scene, two] "GoodBad turns bad"
  ]

main = text (toString (filter (hasTags [goodBad]) notes))

and: (a -> Bool) -> (a -> Bool) -> a -> Bool
and funcA funcB a = (&&) (funcA a) (funcB a)

isA: String -> Tag -> Bool
isA tag = .category >> (==) tag

hasTags: List Tag -> Note -> Bool
hasTags targetTags note =
  let
    noteTags = Set.fromList (map .name (.tags note))
    tags = Set.fromList (map .name targetTags)
    intersection = Set.intersect noteTags tags
  in
    Set.size tags == Set.size intersection

{-hasTagsOfCategory: String -}



chapters = filter (isA "chapter") tags
sides = filter (isA "side") tags

chaptersWithGoodBad =
  let
    rels =  (filter (hasTags [goodBad]) notes)
  in
    tagsInNotesOfCategory "chapter" rels

sidesOfGoodBad =
  let
    rels =  (filter (hasTags [goodBad]) notes)
  in
    tagsInNotesOfCategory "side" rels

{-goodBadChaptersBySide: List (String, List (String, String))-}
goodBadChaptersBySide =
  let
    goodBadChaptersWithSide: Tag -> (Tag, List Note)
    goodBadChaptersWithSide = \side -> (side, filter (hasTags [side, goodBad]) notes)
    goodBadNotesBySide: List (Tag, List Note)
    goodBadNotesBySide = map goodBadChaptersWithSide sides
    noteWithChapter = \note -> (tagsOfCategory "chapter" note, .title note)
    notesChapters = \notesOfOneSide -> map noteWithChapter notes
  in
    map (\(tag, notes) -> (.name tag, notesChapters notes)) goodBadNotesBySide
