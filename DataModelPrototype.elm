module DataModelPrototype exposing (..)

import List exposing (..)
import Set exposing (Set)
import ViewHelpers exposing (..)
import Notes exposing (..)
import Html exposing (text)


main = view sidesOfGoodBad

-- AND of functions
(&>): (a -> Bool) -> (a -> Bool) -> a -> Bool
(&>) funcA funcB a = (&&) (funcA a) (funcB a)

-- Returns the intersection of two tag lists
tagsIntersection: List Tag -> List Tag -> List Tag
tagsIntersection listA listB =
  let
    listToSet = \l -> Set.fromList (map .name l)
  in
    Set.intersect (listToSet listA) (listToSet listB)
      |> Set.toList
      |> map Tag

-- True if the tags of the note contain at least all of the targetTags
hasAllTags: List Tag -> Note -> Bool
hasAllTags targetTags note =
  let
    intersection = tagsIntersection targetTags note.tags
  in
    List.length targetTags == List.length intersection

-- True if the tags of the note contain at least one of the targetTags
hasSomeTags: List Tag -> Note -> Bool
hasSomeTags targetTags note =
  let
    intersection = tagsIntersection targetTags note.tags
  in
    not (List.isEmpty intersection)

-- True if the tags of the note contain targetTag
hasTag: Tag -> Note -> Bool
hasTag targetTag = hasSomeTags [targetTag]

-- Returns Maybe the first common tag of two lists
firstCommonTag: List Tag -> List Tag -> Maybe Tag
firstCommonTag setA setB =
    tagsIntersection setA setB |> List.head

-- Returns Maybe what note the tag belongs to
noteOfTag: Maybe Tag -> Maybe Note
noteOfTag tag =
  case tag of
    Just t -> notes
      |> filter (\n -> n.tag.name == t.name)
      |> List.head
    Nothing -> Nothing

{-| Returns, from a note, its first tag of a given category.
E.g. In a note related to chapter two, `relatedNoteOfTag chapter` returns the
note of chapter two.

Given notes:
  chapterTwo = Note (Tag "chapterTwo") [Tag "chapter"] ""
  relatedNote = Note (Tag "any") [chapterTwo, chapterThree] ""

Usage is as follows:
  relatedNoteOfTag (Tag "chapter") relatedNote == Just chapterTwo
-}
relatedNoteOfTag: Tag -> Note -> Maybe Note
relatedNoteOfTag category note =
  let
    firstCategoryTagInNote = firstCommonTag (tagsTaggedAs category) note.tags
  in
    noteOfTag firstCategoryTagInNote

-- Returns the notes tagged with tag
notesTaggedAs: Tag -> List Note
notesTaggedAs targetTag =
  notes |> filter (hasTag targetTag)

-- Returns the tags of the notes tagged with tag
tagsTaggedAs: Tag -> List Tag
tagsTaggedAs targetTag =
  notesTaggedAs targetTag |> map .tag

type Relationship = Tagged Tag | WithTagOfCategory Tag

relatedNotes: List Relationship -> List Note
relatedNotes relationships =
  let
    filterNotes relationship notes =
      case relationship of
        Tagged tag -> notes |> filter (hasTag tag)
        WithTagOfCategory tag -> notes |> filter (hasSomeTags (tagsTaggedAs tag))
  in
    relationships
      |> List.foldl filterNotes notes





chapterTags = tagsTaggedAs chapter
sideTags = tagsTaggedAs side

chaptersWithGoodBad: List Note
chaptersWithGoodBad = relatedNotes [Tagged goodBad, WithTagOfCategory chapter]

sidesOfGoodBad: List Note
sidesOfGoodBad = relatedNotes [Tagged goodBad, WithTagOfCategory side]


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

goodBadSidesOfChapter: Tag -> List (String, String)
goodBadSidesOfChapter chapter =
  let
    noteToSideWithDescription note =
      case (relatedNoteOfTag side note) of
        Just sideNote -> (sideNote.title, note.title)
        Nothing -> ("", note.title)
  in
    notes
      |> filter (hasAllTags [goodBad, chapter] &> hasSomeTags sideTags)
      |> map noteToSideWithDescription


-- side > chapter (filter goodBad)
goodBadChaptersBySide: List (String, List (String, String))
goodBadChaptersBySide =
  notes
    |> filter (hasTag side)
    |> map (\n -> (n.title, goodBadChaptersOfSide n.tag))

goodBadSidesByChapter: List (String, List (String, String))
goodBadSidesByChapter =
  notes
    |> filter (hasTag chapter)
    |> map (\n -> (n.title, goodBadSidesOfChapter n.tag))
