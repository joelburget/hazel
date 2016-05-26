import { configure } from '@kadira/storybook';

function loadStories() {
  require('./hole');
  require('./computation');
}

configure(loadStories, module);
