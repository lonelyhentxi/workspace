import React, { useState, useLayoutEffect, useRef, useEffect } from "react";

function AutoSizedCurrency({
  amountText,
  maxWidth,
  height,
  initFontSize,
  className
}) {
  const textRef = useRef(null);
  const [visible, setVisible] = useState(false);
  const [fontSize, setFontSize] = useState(initFontSize);
  const [width, setWidth] = useState(maxWidth);

  useLayoutEffect(() => {
    setFontSize(initFontSize);
    setWidth(maxWidth);
    setVisible(false);
  }, [amountText, initFontSize, maxWidth]);

  useEffect(() => {
    /**
     * @type { SVGTextElement }
     */
    const textEl = textRef.current;
    console.log(textEl);
    if (textEl) {
      const computedWidth = textEl.getComputedTextLength();
      if (computedWidth > maxWidth) {
        setFontSize(Math.floor((maxWidth / computedWidth) * initFontSize));
      } else {
        setWidth(computedWidth);
      }
      setVisible(true);
    }
  }, [amountText, initFontSize, maxWidth]);

  return (
    <svg
      version="1.1"
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
    >
      <text
        x={0}
        y={height / 2}
        ref={textRef}
        style={{
          fontSize,
          opacity: visible ? 1 : 0,
          alignmentBaseline: "central"
        }}
      >
        {amountText}
      </text>
    </svg>
  );
}

export default function App() {
  useLayoutEffect(() => {}, []);

  return (
    <div className="App">
      <AutoSizedCurrency
        maxWidth={100}
        height={100}
        initFontSize={16}
        amountText={"880,880,880,880.00"}
      />
    </div>
  );
}
