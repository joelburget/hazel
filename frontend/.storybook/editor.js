import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import {
  convertFromRaw,
  convertToRaw,
  ContentState,
} from 'draft-js';

import Editor from '../Editor';

const onChange = action('onChange');

function contentFromPrimitive({ tag, contents }) {
  switch (tag) {
    case 'Nat':
      const text = '' + contents;
      return [{
        text,
        type: 'unstyled',
        entityRanges: [{
          offset: 0,
          length: text.length,
          key: tag,
        }],
      }];

    case 'String':
      return [{
        text: contents,
        type: 'unstyled',
        entityRanges: [{
          offset: 0,
          length: contents.length,
          key: tag,
        }],
      }];

    default:
      throw Error('unhandled primitive tag');
  }
}

function contentFromComputation({ tag, contents }) {
  switch (tag) {
    case 'FVar':
      return [{
        text: contents,
        type: 'unstyled',
        entityRanges: [{
          offset: 0,
          length: contents.length,
          key: tag,
        }],
      }];

    case 'Primitive':
      return contentFromPrimitive(contents);

    default:
      throw Error('unhandled computation tag');
  }
};

function convertFromComputation(computation) {
  return convertFromRaw({
    blocks: contentFromComputation(computation),
    entityMap: {
      FVar: {
        type: 'TOKEN',
        mutability: 'MUTABLE',
        data: {type: 'FVar'},
      },
      Primitive: {
        type: 'TOKEN',
        mutability: 'MUTABLE',
        data: {type: 'Primitive'},
      },
    },
  });
}

storiesOf('Editor', module)
  .add('empty', () => {
    const rawContent = {
      blocks: [
        {
          text: '',
          type: 'unstyled'
        }
      ],
      entityMap: {},
    };
    const content = convertFromRaw(rawContent);
    return <Editor onChange={onChange} content={content} />;
  }).add('(FVar "x")', () => {
    const content = convertFromComputation({
      tag: 'FVar',
      contents: 'x',
    });
    return <Editor onChange={onChange} content={content} />;
  }).add('(Primitive (Nat 5))', () => {
    const content = convertFromComputation({
      tag: 'Primitive',
      contents: {
        tag: 'Nat',
        contents: 5,
      },
    });
    return <Editor onChange={onChange} content={content} />;
  }).add('(Primitive (String "hazel"))', () => {
    const content = convertFromComputation({
      tag: 'Primitive',
      contents: {
        tag: 'String',
        contents: "hazel",
      },
    });
    return <Editor onChange={onChange} content={content} />;
  });
