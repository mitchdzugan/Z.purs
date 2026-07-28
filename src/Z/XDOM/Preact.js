import { Fragment, h, Component } from "preact";

class StateComponent extends Component {
  constructor(props) {
    super(props);
    this.state = { st: props.initialState };
  }

  render() {
    const setter = (st) => {
      return this.props.pure(this.setState({ st }));
    };
    return h(Fragment, {}, ...this.props.renderEls(this.state.st)(setter));
  }
}

const KeyComponent = ({ children }) => {
  return children;
};

export const js_textEl = (s) => s;
export const js_renderFragment = (children) => h(Fragment, {}, ...children);
export const js_renderEl = (el) => (props) => (children) =>
  h(el, props, ...children);
export const js_propsFromPropWs = (getK) => (getV) => (kvs) => {
  const res = {};
  for (const kv of kvs) {
    res[getK(kv)] = getV(kv);
  }
  return res;
};
export const js_withState = (pure) => (renderEls) => (initialState) =>
  h(StateComponent, { renderEls, initialState, pure });

export const js_strict = (el) => el;
