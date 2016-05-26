import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import Editor from '../Editor';

storiesOf('Editor', module)
  .add('... basic...', () => (
    <Editor action={action} />
  ))
