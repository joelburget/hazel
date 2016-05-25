import React from 'react';
import {
  CompositeDecorator,
  ContentState,
  Editor,
  EditorState,
} from 'draft-js';

const computationHeads = [
  'bvar',
  'hole', // XXX (fvar)
  'app',
  'annot',
  'case',
  'choose',
  'unpack',
];
const cHeadRegex = new RegExp(computationHeads.join("|"), 'g');

const valueHeads = [
  'lam',
  'primop',
  'let',
  'primitive',
  'index',
  'neu',
  'tuple',
  'plus',
];
const vHeadRegex = new RegExp(valueHeads.join("|"), 'g');

function cHeadStrategy(contentBlock, callback) {
  findWithRegex(cHeadRegex, contentBlock, callback);
}

function vHeadStrategy(contentBlock, callback) {
  findWithRegex(vHeadRegex, contentBlock, callback);
}

function findWithRegex(regex, contentBlock, callback) {
  const text = contentBlock.getText();
  let matchArr, start;
  while ((matchArr = regex.exec(text)) !== null) {
    start = matchArr.index;
    callback(start, start + matchArr[0].length);
  }
}

const Computation = props => {
  return (
    <span {...props} style={styles.computation}>{props.children}</span>
  );
}

const Value = props => {
  return (
    <span {...props} style={styles.value}>{props.children}</span>
  );
}

// TODO: shouldn't this just be a DraftDecorator, since it's not composing
// decorators?
// https://github.com/facebook/draft-js/blob/master/src/model/decorators/DraftDecoratorType.js
const ComputationDecorator = new CompositeDecorator([
  {
    strategy: cHeadStrategy,
    component: Computation,
  },
  {
    strategy: vHeadStrategy,
    component: Value,
  },
]);

// editor to fill a hole where a computation is expected
export default class HazelEditor extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      editorState: EditorState.createWithContent(
        ContentState.createFromText('lam(bvar(0))'),
        ComputationDecorator
      )
    };
    this.onChange = change => this._onChange(change);
  }

  _onChange(editorState) {
    this.props.action('change')(editorState.getCurrentContent());
    this.setState({editorState})
  }

  render() {
    const {editorState} = this.state;
    return (
      <div style={styles.root}>
        <Editor editorState={editorState} onChange={this.onChange} />
      </div>
    );
  }
}

const styles = {
  root: {
    fontFamily: 'monospace',
  },
  computation: {
    color: '#595BE4',
  },
  value: {
    color: '#159CF1',
  },
};
