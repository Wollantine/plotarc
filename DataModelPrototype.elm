module DataModelPrototype exposing (..)

import List exposing (..)
import Set exposing (Set)
import ViewHelpers exposing (..)
import Notes exposing (..)


tagsInNote: Note -> List Tag
tagsInNote {tags} = tags

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
