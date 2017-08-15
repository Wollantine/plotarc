const
  chapter = 'chapter',
  scene = 'scene',
  storyArc = 'storyArc',
  side = 'side',
  character = 'character',
  object = 'object',
  place = 'place',
  event = 'event',
  time = 'time';

const notes = [
  {title: 'GoodBad', relationships: [character]},
  {title: 'BadBad', relationships: [character]},
  {title: 'Good', relationships: [side]},
  {title: 'Bad', relationships: [side]},
  {title: '1', relationships: [chapter]},
  {title: '2', relationships: [chapter]},
  {relationships: ['GoodBad', 'Good', '1']},
  {relationships: ['GoodBad', 'Bad', '2']},
  {relationships: ['BadBad', 'Bad', '1']},
  {relationships: ['BadBad', 'Bad', '2']},
  {title: '1.1', description: 'GoodBad turns bad', relationships: [scene]},
]


const log = x => {console.log(x); return x;}

const intersection = setA => setB =>
  setA.filter(a => setB.indexOf(a) !== -1);
const is = dimension => note =>
  note.relationships.indexOf(dimension) !== -1;
const has = dimensions => note =>
  intersection(note.relationships)(dimensions).length === dimensions.length;
const title = n => n.title, relationship = n => n.relationships;
const titles = notes => notes.map(title), relationships = notes => notes.map(relationship);


const chapters = titles(notes.filter(is(chapter)));
const chaptersWithGoodBad = chapters.reduce((chapters, c) => [...chapters, ...notes.filter(has(['GoodBad', c]))], []);
const sides = titles(notes.filter(is(side)));
const goodBadSides = sides.reduce((sides, s) => [...sides, notes.filter(has(['GoodBad', s]))], []);

const GoodBadsChaptersBySide = sides.map(side => {
  const sideChapters = chaptersWithGoodBad.filter(is(side));
  const chapterTitles = sideChapters.map(c => intersection(relationship(c))(chapters)[0])
  return {
    side,
    chapters: chapterTitles,
  };
});

const GoodBadsSideByChapter = chapters.map(chapter => {
  const chapterSides = chapters
});

console.log(GoodBadsChaptersBySide);
