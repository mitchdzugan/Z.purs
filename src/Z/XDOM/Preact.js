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

class DidMountComponent extends Component {
  postRender() {}

  componentDidMount() {
    this.props.onMount();
    this.postRender();
  }

  componentDidUpdate() {
    this.postRender();
  }
}

class EffComponent extends Component {
  postRender() {
    const isNew = !this.last || !this.props.eq(this.last[0])(this.props.v);
    if (!isNew) {
      return;
    }
    const lastFn = !this.last ? () => {} : this.last[1];
    lastFn();
    this.last = [this.props.v, this.props.onNew()];
  }

  componentDidMount() {
    this.postRender();
  }

  componentDidUpdate() {
    this.postRender();
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
export const js_withState = (pure) => (renderEls) => (initialState) =>
  h(StateComponent, { renderEls, initialState, pure });
export const js_withKey = (key) => (el) => h(KeyComponent, { key, el });
export const js_didMountEl = (onMount) => h(DidMountComponent, { onMount });

class BoundedError {
  constructor(error) {
    this.error = error;
  }
}

function BoundedErrorComponent(props) {
  try {
    return props.renderChild();
  } catch (err) {
    if (!(err instanceof BoundedError)) {
      throw err;
    }
    return props.renderError(err.error);
  }
}

export const js_withBoundedError = (renderError) => (renderChild) =>
  h(BoundedErrorComponent, { renderError, renderChild });

export const js_throwBoundedError = (e) => {
  throw new BoundedError(e);
};
