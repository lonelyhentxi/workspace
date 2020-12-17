import React, {
  useCallback,
  useRef,
  useLayoutEffect,
  useState,
  useEffect
} from "react";
import "./styles.css";
import classNames from "classnames";

function elementScrollTo(element, { left, top, behavior }) {
  if (!element) {
    return;
  }
  const polyfillElementScrollTo = (element, { left, top }) => {
    if (typeof left === "number" && !Number.isNaN(left)) {
      element.scrollLeft = left;
    }
    if (typeof top === "number" && !Number.isNaN(top)) {
      element.scrollTop = top;
    }
  };

  if (typeof element.scrollTo === "function") {
    try {
      element.scrollTo({ left, top, behavior });
    } catch (e) {
      polyfillElementScrollTo(element, { left, top });
    }
  } else {
    polyfillElementScrollTo(element, { left, top });
  }
}

function Tabs({ tabs, onSelect, selected: outSelected, className }) {
  const containerRef = useRef(null);
  const tabRefs = useRef(new Array(tabs.length).fill(null));
  const [selected, setSelected] = useState(0);
  const [selectedDecorationStyles, setSelectedDecorationStyles] = useState({
    visible: false,
    left: 0,
    width: 0
  });

  useLayoutEffect(() => {
    if (tabs.length !== tabRefs.current.length) {
      tabRefs.current = new Array(tabs.length).fill(null);
      setSelected(0);
      setSelectedDecorationStyles((v) => ({ ...v, visible: false }));
    }
  }, [tabs]);

  useLayoutEffect(() => {
    if (
      outSelected !== selected &&
      outSelected >= 0 &&
      outSelected < tabs.length
    ) {
      setSelected(outSelected);
    }
  }, [outSelected]);

  useEffect(() => {
    /**
     * @type { HTMLElement }
     */
    const container = containerRef.current;
    /**
     * @type { HTMLElement }
     */
    const tab = tabRefs.current[selected];
    if (container && tab) {
      const containerClientWidth = container.getBoundingClientRect().width;
      const tabMiddle = tab.offsetLeft + tab.getBoundingClientRect().width / 2;
      const scrollLeftWhenTabInMiddle = tabMiddle - containerClientWidth / 2;
      elementScrollTo(container, {
        left: scrollLeftWhenTabInMiddle,
        behavior: "smooth"
      });
      const tabText = tab.querySelector(".tab-text");
      if (tabText) {
        const width = tabText.getBoundingClientRect().width;
        const left = tabMiddle - width / 2;
        setSelectedDecorationStyles({ left, width, visible: true });
      }
    }
  }, [selected]);

  const handleSelect = useCallback(
    (t, i) => () => {
      typeof onSelect === "function" && onSelect(t, i);
    },
    [onSelect]
  );

  return (
    <div className={classNames("tabs", className)} ref={containerRef}>
      {tabs.map((t, i) => (
        <div
          className={classNames("tab", { "tab-selected": selected === i })}
          ref={(v) => (tabRefs.current[i] = v)}
          onClick={handleSelect(t, i)}
          key={i}
        >
          <div className="tab-text">{t}</div>
        </div>
      ))}
      <div className="selected-decoration" style={selectedDecorationStyles} />}
    </div>
  );
}

export default function App() {
  const [tabs, setTabs] = useState([
    "aaaa",
    "bbbb",
    "cccccc",
    "dddd",
    "efg",
    "abcdge",
    "114514114514114514",
    "sadjfafds",
    "aaaa",
    "ffsfajsa",
    "dddd"
  ]);
  const [selected, setSelected] = useState(0);

  const handleSelect = useCallback((t, i) => {
    setSelected(i);
  }, []);

  return (
    <div className="App">
      <Tabs tabs={tabs} onSelect={handleSelect} selected={selected} />
      <div>{tabs[selected]}</div>
    </div>
  );
}
