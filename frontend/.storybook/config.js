import { configure } from '@kadira/storybook';

function loadStories() {
  require('./hole');
  require('./editor');
}

configure(loadStories, module);
