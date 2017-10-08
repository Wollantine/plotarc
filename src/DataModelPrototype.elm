module DataModelPrototype exposing (..)

import List exposing (..)
import Set exposing (Set)
import View exposing (..)
import Model exposing (..)
import Html exposing (text)
import Maybe.Extra exposing (isNothing, values)


{-| AND of functions
-}
(&>): (a -> Bool) -> (a -> Bool) -> a -> Bool
(&>) funcA funcB a = (&&) (funcA a) (funcB a)

{-| Returns the intersection of two tag lists
-}
tagsIntersection: List Tag -> List Tag -> List Tag
tagsIntersection listA listB =
  let
    listToSet = \l -> Set.fromList (map .name l)
  in
    Set.intersect (listToSet listA) (listToSet listB)
      |> Set.toList
      |> map Tag

{-| True if the tags of the note contain at least all of the targetTags
-}
hasAllTags: List Tag -> Note -> Bool
hasAllTags targetTags note =
  let
    intersection = tagsIntersection targetTags note.tags
  in
    List.length targetTags == List.length intersection

{-| True if the tags of the note contain at least one of the targetTags
-}
hasSomeTags: List Tag -> Note -> Bool
hasSomeTags targetTags note =
  let
    intersection = tagsIntersection targetTags note.tags
  in
    not (List.isEmpty intersection)

{-| True if the tags of the note contain targetTag
-}
hasTag: Tag -> Note -> Bool
hasTag targetTag = hasSomeTags [targetTag]

{-| Returns Maybe the first common tag of two lists
-}
firstCommonTag: List Tag -> List Tag -> Maybe Tag
firstCommonTag setA setB =
    tagsIntersection setA setB |> List.head

{-| Returns Maybe what note the tag belongs to
-}
noteOfMaybeTag: List Note -> Maybe Tag -> Maybe Note
noteOfMaybeTag notes tag =
  case tag of
    Just t -> notes
      |> filter (\n -> n.tag.name == t.name)
      |> List.head
    Nothing -> Nothing

noteOfTag: List Note -> Tag -> Maybe Note
noteOfTag notes tag = noteOfMaybeTag notes (Just tag)

{-| Returns, from a note, its first tag of a given category.
E.g. In a note related to chapter two, `relatedNoteOfTag chapter` returns the
note of chapter two.

Given notes:
```elm
  chapterTwo = Note (Tag "chapterTwo") [Tag "chapter"] ""
  relatedNote = Note (Tag "any") [chapterTwo, chapterThree] ""
```

Usage is as follows:
```elm
  relatedNoteOfTag (Tag "chapter") relatedNote == Just chapterTwo
```
-}
relatedNoteOfTag: List Note -> Tag -> Note -> Maybe Note
relatedNoteOfTag notes category note =
  let
    firstCategoryTagInNote = firstCommonTag (tagsTaggedAs notes category) note.tags
  in
    noteOfMaybeTag notes firstCategoryTagInNote

{-| Returns the notes tagged with tag
-}
notesTaggedAs: List Note -> Tag -> List Note
notesTaggedAs notes targetTag =
  notes |> filter (hasTag targetTag)

{-| Returns the tags of the notes tagged with tag
-}
tagsTaggedAs: List Note -> Tag -> List Tag
tagsTaggedAs notes targetTag =
  notesTaggedAs notes targetTag |> map .tag

{-| Returns True only if group has no belonging notes
-}
isEmptyGroup: {a | group: List Note} -> Bool
isEmptyGroup g = List.isEmpty g.group

{-| Kinds of relationships between notes and tags:

- Tagged tag: Notes that have tag in their tags.
- WithTagOfCategory category: Notes that have a tag in their tags such that
tag's note has category in their tags.
-}
type Relationship = Tagged Tag | WithTagOfCategory Tag | WithTagOfSuperCategory Tag

{-| Returns the notes that fulfill the list of relationships in notes.
E.g.

Given notes:
```elm
  chapterTwo = Note (Tag "chapterTwo") [Tag "chapter"] ""
  relatedNote1 = Note (Tag "any") [chapterOne] ""
  relatedNote2 = Note (Tag "any") [chapterTwo] ""
```

Usage is as follows:
```elm
  relatedNotes [Tagged (Tag "chapterTwo")] notes == [relatedNote2]
  relatedNotes [WithTagOfCategory (Tag "chapter")] notes == [relatedNote1, relatedNote2]
```
-}
relatedNotes: List Relationship -> List Note -> List Note
relatedNotes relationships notes =
  let
    tagsTaggedInNotesAs = tagsTaggedAs notes
    filterNotes relationship notes =
      case relationship of
        Tagged tag -> notes |> filter (hasTag tag)
        WithTagOfCategory tag -> notes |> filter (hasSomeTags (tagsTaggedInNotesAs tag))
        WithTagOfSuperCategory tag -> notes
          |> filter (hasSomeTags (notes
            |> filter (hasSomeTags (tagsTaggedInNotesAs tag))
            |> map .tag
          ))
  in
    relationships
      |> List.foldl filterNotes notes

{-| Groups notes in notes by their first tag tagged as category.
Returns a list of groups. For each group, its title is the tag's note,
and the notes are the ones that have that tag.
E.g. Groups notes by their first tag of category "chapter".

Given notes:
```elm
  chapterOne = Note (Tag "chapterOne") [Tag "chapter"] ""
  chapterTwo = Note (Tag "chapterTwo") [Tag "chapter"] ""
  relatedNote1 = Note (Tag "any") [chapterOne] ""
  relatedNote2 = Note (Tag "any") [chapterOne] ""
```

Usage is as follows:
```elm
  groupNotesBy (Tag "chapter") [relatedNote1, relatedNote2] ==
    [ {groupTitle: chapterOne, groupNotes: [relatedNote1, relatedNote2]}
    , {groupTitle: chapterTwo, groupNotes: []}
    ]
```
-}
groupNotesBy: Tag -> List Note -> List GroupOfNotes
groupNotesBy category notes =
  let
    tags = tagsTaggedAs notes category
    groupOfNotes: Tag -> Maybe GroupOfNotes
    groupOfNotes tag = notes
      |> filter (hasTag tag)
      |> \ns -> case (noteOfTag notes tag) of
        Just note -> Just (GroupOfNotes note ns)
        Nothing -> Nothing
  in
    tags
      |> map groupOfNotes
      |> values


groupNotesByMultipleTags: List Tag -> List Note -> List MultigroupOfNotes
groupNotesByMultipleTags categories notes =
  let
    groupingNotes tags = tagsAsGroupingNotes notes tags
    multigroupOfNotes tags = notes
      |> filter (hasAllTags tags)
      |> \ns -> MultigroupOfNotes (groupingNotes tags) ns
  in
    categories
      |> map (tagsTaggedAs notes)
      |> cartesianProduct
      |> map multigroupOfNotes
      |> filter (not << isEmptyGroup)

tagsAsGroupingNotes: List Note -> List Tag -> List Note
tagsAsGroupingNotes notes tags =
  tags
    |> map (noteOfTag notes)
    |> values


cartesianProduct: List (List a) -> List (List a)
cartesianProduct lists = cartesianWithValues [] lists

cartesianWithValues: List a -> List (List a) -> List (List a)
cartesianWithValues selectedValues lists =
  case lists of
    [] -> [selectedValues]
    firstList :: restOfLists ->
      firstList
        |> map (\e -> cartesianWithValues (e :: selectedValues) restOfLists)
        |> concat

{-
- Relationships with more than 2 hierarchy levels? (e.g. scenes by book,
  characters by chapter when characters are organized by scene)
- Relationships down the hierarchy? (e.g. characters by scene when characters
  are organized by chapter... can only be assumed all characters in a chapter
  appear in all chapter's scenes.)
-}

main = multigroupsView (groupNotesByMultipleTags [chapter, character] notes)

-- QUERY TESTS

chapterTags = tagsTaggedAs notes chapter
sideTags = tagsTaggedAs notes side

chaptersWithGoodBad: List Note
chaptersWithGoodBad = relatedNotes [Tagged goodBad, WithTagOfCategory chapter] notes

sidesOfGoodBad: List Note
sidesOfGoodBad = relatedNotes [Tagged goodBad, WithTagOfCategory side] notes

goodBadChaptersBySide: List GroupOfNotes
goodBadChaptersBySide = groupNotesBy side chaptersWithGoodBad

goodBadSidesByChapter: List GroupOfNotes
goodBadSidesByChapter = groupNotesBy chapter sidesOfGoodBad

scenesByChapter = groupNotesBy chapter (relatedNotes [Tagged scene] notes)

chaptersWithBadBadAndRingOfPower = relatedNotes
  [ Tagged badBad
  , Tagged ringOfPower
  , WithTagOfCategory chapter
  ]
  notes

notesByChapter = groupNotesBy chapter notes
