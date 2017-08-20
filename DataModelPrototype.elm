module DataModelPrototype exposing (..)

import List exposing (..)
import Set exposing (Set)
import ViewHelpers exposing (..)
import Notes exposing (..)
import Html exposing (text)


main = groupsView goodBadChaptersBySide

(&>): (a -> Bool) -> (a -> Bool) -> a -> Bool
(&>) funcA funcB a = (&&) (funcA a) (funcB a)

tagsInNote: Note -> List Tag
tagsInNote {tags} = tags

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

firstTagOf: List Tag -> Note -> Maybe Tag
firstTagOf tags note =
    tagsIntersection tags note.tags |> List.head

noteOfTag: Maybe Tag -> List Note -> Maybe Note
noteOfTag tag notes =
  case tag of
    Just t -> notes
      |> filter (\n -> n.tag.name == t.name)
      |> List.head
    Nothing -> Nothing

relatedNoteOfTag: Tag -> Note -> Maybe Note
relatedNoteOfTag tag note =
  let
    notesOfTagType = notes |> filter (hasSomeTags [tag])
    tagsOfTagType = map .tag notesOfTagType
    firstTagOfTagTypeInNote = firstTagOf tagsOfTagType note
  in
    notesOfTagType |> noteOfTag firstTagOfTagTypeInNote


chapterTags = map .tag (filter (hasSomeTags [chapter]) notes)
sideTags = map .tag (filter (hasSomeTags [side]) notes)

chaptersWithGoodBad: List Note
chaptersWithGoodBad =
  let
    isAChapterWithGoodBad = (hasAllTags [goodBad]) &> (hasSomeTags chapterTags)
  in
    filter isAChapterWithGoodBad notes

sidesOfGoodBad: List Note
sidesOfGoodBad =
  let
    isASideOfGoodBad = (hasAllTags [goodBad]) &> (hasSomeTags sideTags)
  in
    filter isASideOfGoodBad notes


goodBadChaptersOfSide: Tag -> List (String, String)
goodBadChaptersOfSide side =
  let
    noteToChapterWithDescription note =
      case (relatedNoteOfTag chapter note) of
        Just chapterNote -> (chapterNote.title, note.title)
        Nothing -> ("", note.title)
  in
    notes
      |> filter (hasAllTags [goodBad, side] &> hasSomeTags chapterTags)
      |> map noteToChapterWithDescription



goodBadChaptersBySide: List (String, List (String, String))
goodBadChaptersBySide =
  notes
    |> filter (hasSomeTags [side])
    |> map (\n -> (n.title, goodBadChaptersOfSide n.tag))

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
