const { useState, useRef, useCallback, useLayoutEffect, useEffect } = React;

const doPromise = promise => {
  promise.then(() => {});
};

const App = () => {
  const [nodes, setNodes] = useState(() => new Array(100).fill(0).map((_, index) => index));
  const [opacity, setOpacity] = useState(1);
  const nodeRefs = useRef([]);
  const gridsRef = useRef(null);
  const handleReverse = () => {
    const oldPositions = nodeRefs.current.map(v => {
      const rec = v.getBoundingClientRect();
      return { left: rec.left, top: rec.top };
    });
    const newNodes = nodes.map(v => 100 - v);
    setNodes(newNodes);
    requestAnimationFrame(() => {
      nodeRefs.current.forEach((v, i) => {
        const old = oldPositions[100 - i - 1];
        const now = v.getBoundingClientRect();
        const keyframes = [{ transform: `translate(${old.left - now.left}px,${old.top - now.top}px)` }, { transform: 'translate(0, 0)' }];
        v.animate(keyframes, { duration: 1000, easing: 'cubic-bezier(0, 0, 0.32, 1)' });
      });
    });
  };
  const handleDisappear = () => {
    const el = gridsRef.current;
    const oldOpacity = el.style.opacity;
    setOpacity(0);
    requestAnimationFrame(() => {
      const animation = el.animate([{ opacity: oldOpacity }, { opacity: el.style.opacity }], {
        duration: 1000,
        easing: 'ease' });

    });
  };
  const handleAppear = () => {
    const el = gridsRef.current;
    const oldOpacity = el.style.opacity;
    setOpacity(1);
    requestAnimationFrame(() => {
      const animation = el.animate([{ opacity: oldOpacity }, { opacity: el.style.opacity }], {
        duration: 1000,
        easing: 'ease' });

    });
  };
  return /*#__PURE__*/(
    React.createElement("div", null, /*#__PURE__*/
    React.createElement("div", null, /*#__PURE__*/
    React.createElement("button", { onClick: handleReverse }, "reverse"), /*#__PURE__*/
    React.createElement("button", { onClick: handleDisappear }, "disappear"), /*#__PURE__*/
    React.createElement("button", { onClick: handleAppear }, "appear")), /*#__PURE__*/

    React.createElement("div", { className: "grids", ref: gridsRef, style: { opacity } },
    nodes.map((v, i) => /*#__PURE__*/React.createElement("div", { className: "grid", key: v, ref: el => nodeRefs.current[i] = el }, v)))));



};

ReactDOM.render( /*#__PURE__*/React.createElement(App, null), document.getElementById('app'));