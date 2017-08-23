module DataModelPrototype exposing (..)

import List exposing (..)
import Set exposing (Set)
import ViewHelpers exposing (..)
import Notes exposing (..)
import Html exposing (text)


-- AND of functions
(&>): (a -> Bool) -> (a -> Bool) -> a -> Bool
(&>) funcA funcB a = (&&) (funcA a) (funcB a)

emptyNote: Note
emptyNote = Note (Tag "") [] ""

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
```elm
  chapterTwo = Note (Tag "chapterTwo") [Tag "chapter"] ""
  relatedNote = Note (Tag "any") [chapterTwo, chapterThree] ""
```

Usage is as follows:
```elm
  relatedNoteOfTag (Tag "chapter") relatedNote == Just chapterTwo
```
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

{-| Kinds of relationships between notes and tags:

- Tagged tag: Notes that have tag in their tags.
- WithTagOfCategory category: Notes that have a tag in their tags such that
tag's note has category in their tags.
-}
type Relationship = Tagged Tag | WithTagOfCategory Tag

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
  relatedNotes [Tagged (Tag "chapterTwo")] notes = [relatedNote2]
  relatedNotes [WithTagOfCategory (Tag "chapter")] notes = [relatedNote1, relatedNote2]
```
-}
relatedNotes: List Relationship -> List Note -> List Note
relatedNotes relationships notes =
  let
    filterNotes relationship notes =
      case relationship of
        Tagged tag -> notes |> filter (hasTag tag)
        WithTagOfCategory tag -> notes |> filter (hasSomeTags (tagsTaggedAs tag))
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
  groupNotesBy (Tag "chapter") [relatedNote1, relatedNote2] =
    [ {groupTitle: chapterOne, groupNotes: [relatedNote1, relatedNote2]}
    , {groupTitle: chapterTwo, groupNotes: []}
    ]
```
-}
groupNotesBy: Tag -> List Note -> List GroupOfNotes
groupNotesBy category notes =
  let
    tags = tagsTaggedAs category
    title tag = case (noteOfTag tag) of
      Just n -> n
      Nothing -> emptyNote
    groupOfNotes tag = notes
      |> filter (hasTag tag)
      |> \ns -> GroupOfNotes (title (Just tag)) ns
  in
    tags |> map groupOfNotes

{-
- Group notes by multiple tags?
- Relationships with more than 2 hierarchy levels? (e.g. scenes by book,
  characters by chapter when characters are organized by scene)
- Relationships down the hierarchy? (e.g. characters by scene when characters
  are organized by chapter... can only be assumed all characters in a chapter
  appear in all chapter's scenes.)
-}

main = groupsView notesByChapter

chapterTags = tagsTaggedAs chapter
sideTags = tagsTaggedAs side

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
