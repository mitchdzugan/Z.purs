import { Fragment, h, Component } from "preact";

class StateComponent extends Component {
  constructor(props) {
    super(props);
    this.state = { st: props.initialState };
  }

  render() {
    const setter = (st) => {
      return () => this.setState({ st });
    };
    return h(Fragment, {}, ...this.props.renderEls(this.state.st)(setter));
  }
}

class EffComponent extends Component {
  get lastFn() {
    return !this.last ? () => {} : () => this.last[1]();
  }

  postRender() {
    const isNew = !this.last || !this.props.eq(this.last[0])(this.props.v);
    if (!isNew) {
      return;
    }
    this.lastFn();
    this.last = [this.props.v, this.props.onNew()];
  }

  componentDidMount() {
    this.postRender();
  }

  componentDidUpdate() {
    this.postRender();
  }

  componentWillUnmount() {
    this.lastFn();
    delete this.last;
  }
}

const KeyComponent = ({ el }) => {
  return el;
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
export const js_withState = (renderEls) => (initialState) =>
  h(StateComponent, { renderEls, initialState });
export const js_withKey = (key) => (el) => h(KeyComponent, { key, el });
export const js_effComponent = (eq) => (v) => (onNew) =>
  h(EffComponent, { eq, v, onNew });

class BoundedError {
  constructor(at, error) {
    this.error = error;
    this.at = at;
  }
}

function BoundedErrorComponent(props) {
  try {
    return props.renderChild();
  } catch (err) {
    if (!(err instanceof BoundedError) || err.at !== props.at) {
      throw err;
    }
    return props.renderError(err.error)();
  }
}

export const js_withBoundedError = (at) => (renderError) => (renderChild) =>
  h(BoundedErrorComponent, { at, renderError, renderChild });

export const js_throwBoundedError = (at) => (e) => {
  throw new BoundedError(at, e);
};
