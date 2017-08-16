const dim = (type, name) => ({type, name});
const isType = type => dim => dim.type === type;
const eqDim = (a, b) => a.type === b.type && a.name === b.name;
const type = dim => dim.type
const types = dims => dims.map(type)

const
  chapter = dim('root', 'chapter'),
  scene = dim('root', 'scene'),
  storyArc = dim('root', 'storyArc'),
  side = dim('root', 'side'),
  character = dim('root', 'character'),
  object = dim('root', 'object'),
  place = dim('root', 'place'),
  event = dim('root', 'event'),
  time = dim('root', 'time');

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
  {title: '1.1', description: 'GoodBad turns bad', relationships: [scene, dim('chapter', '1')]},
]


const log = x => {console.log(x); return x;}

const intersection = setA => setB =>
  setA.filter(a => setB.indexOf(a) !== -1);
const is = dimension => note =>
  types(note.relationships).indexOf(type(dimension)) !== -1;
const has = dimensions => note =>
  intersection(types(note.relationships))(types(dimensions)).length === dimensions.length;
const title = n => n.title, relationship = n => n.relationships;
const titles = notes => notes.map(title), relationships = notes => notes.map(relationship);


const chapters = titles(notes.filter(is(chapter)));
const sides = titles(notes.filter(is(side)));

const chaptersWithGoodBad = chapters.reduce((chapters, c) =>
  [...chapters, ...notes.filter(has(['GoodBad', c]))], []);

const goodBadSides = sides.reduce((sides, s) =>
  [...sides, notes.filter(has(['GoodBad', s]))], []);

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

console.log(chapters);
console.log(sides);
console.log(notes.filter(has(['GoodBad', '1'])));
