import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import Computation from '../Computation';

storiesOf('Computation', module)
  .add('... basic...', () => (
    <Computation action={action} />
  ))
