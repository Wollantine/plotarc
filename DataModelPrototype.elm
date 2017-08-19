module DataModelPrototype exposing (..)

import List exposing (..)
import Set exposing (Set)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

type alias Tag = {name: String}

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
  { tag: Tag
  , tags: List Tag
  , title: String
  }

tagsInNote: Note -> List Tag
tagsInNote {tags} = tags


notes: List Note
notes =
  [ Note one [chapter] "1"
  , Note two [chapter] "2"
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

view: List Note -> Html Never
view notes =
  div [] (List.map noteView notes)

noteView: Note -> Html Never
noteView note =
  div [] [
    text note.title
  ]

groupsView: List (String, List (String, String)) -> Html Never
groupsView groups =
  let
    line = \(title, description) -> div []
      [text (title ++ "   " ++ description)]
    group = \(title, g) -> div []
      [ text title
      , div [style [("margin-left", "20px")]] (map line g)
      ]
  in
    div [] (map group groups)


main = groupsView goodBadChaptersBySide

(&>): (a -> Bool) -> (a -> Bool) -> a -> Bool
(&>) funcA funcB a = (&&) (funcA a) (funcB a)


{- Deprecated: isA implies hierarchy, but that is what hasSomeTags is for.

isA: Tag -> Note -> Bool
isA tag note = tag.name == note.tag.name-}

tagsIntersection: List Tag -> List Tag -> List Tag
tagsIntersection listA listB =
  let
    listToSet = \l -> Set.fromList (map .name l)
  in
    Set.intersect (listToSet listA) (listToSet listB)
      |> Set.toList
      |> map Tag

hasAllTags: List Tag -> Note -> Bool
hasAllTags targetTags note =
  let
    intersection = tagsIntersection targetTags note.tags
  in
    List.length targetTags == List.length intersection

hasSomeTags: List Tag -> Note -> Bool
hasSomeTags targetTags note =
  let
    intersection = tagsIntersection targetTags note.tags
  in
    not (List.isEmpty intersection)




chaptersWithGoodBad: List Note
chaptersWithGoodBad =
  let
    chapterTags = map .tag (filter (hasSomeTags [chapter]) notes)
    isAChapterWithGoodBad = (hasAllTags [goodBad]) &> (hasSomeTags chapterTags)
  in
    filter isAChapterWithGoodBad notes

sidesOfGoodBad: List Note
sidesOfGoodBad =
  let
    sideTags = map .tag (filter (hasSomeTags [side]) notes)
    isASideOfGoodBad = (hasAllTags [goodBad]) &> (hasSomeTags sideTags)
  in
    filter isASideOfGoodBad notes

goodBadChaptersBySide: List (String, List (String, String))
goodBadChaptersBySide =
  [("good", [("1", "blabla1"), ("2", "blabla2")]), ("bad", [("3", "blabla3"), ("4", "blabla4")])]
{-goodBadChaptersBySide =
  let
    goodBadChaptersWithSide: Tag -> (Tag, List Note)
    goodBadChaptersWithSide = \side -> (side, filter (hasTags [side, goodBad]) notes)
    goodBadNotesBySide: List (Tag, List Note)
    goodBadNotesBySide = map goodBadChaptersWithSide sides
    noteWithChapter = \note -> (tagsOfCategory "chapter" note, .title note)
    notesChapters = \notesOfOneSide -> map noteWithChapter notes
  in
    map (\(tag, notes) -> (.name tag, notesChapters notes)) goodBadNotesBySide
    -}
