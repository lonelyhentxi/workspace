import React, { useMemo, useRef, forwardRef, useCallback } from "react";
import "./styles.css";

const scan = (f, initial, array) => {
  const result = [];
  let lastScan = initial;
  for (const curr of array) {
    const currScan = f(lastScan, curr);
    result.push(currScan);
    lastScan = currScan;
  }
  return result;
};

const getSectorClip = (angle) => {
  return `polygon(50% 50%, 0 ${
    50 + 50 * Math.tan(((angle - 180) * Math.PI) / 360)
  }%, 0 0, 100% 0, 100% ${
    50 + 50 * Math.tan(((angle - 180) * Math.PI) / 360)
  }%)`;
};
const getSectorOffset = (rearSideOffset, angle) => rearSideOffset - angle / 2;

const Drawable = forwardRef(({ sectors, className }, ref) => {
  const rearSideOffsets = useMemo(
    () =>
      scan(
        (acc, curr) => acc + curr.angle,
        sectors.length > 0 ? 0 - sectors[0].angle / 2 : 0,
        sectors
      ),
    [sectors]
  );

  return (
    <div ref={ref} className={`drawable${className ? " " + className : ""}`}>
      {sectors.map((s, i) => (
        <div
          className="sector"
          key={i}
          style={{
            clipPath: getSectorClip(s.angle),
            backgroundColor: s.color,
            transform: `rotateZ(${getSectorOffset(
              rearSideOffsets[i],
              s.angle
            )}deg)`
          }}
        >
          <div className="sector-content">{s.color}</div>
        </div>
      ))}
    </div>
  );
});

export default function App() {
  const sectorsRef = useRef([
    {
      angle: 30,
      color: "red"
    },
    {
      angle: 60,
      color: "yellow"
    },
    {
      angle: 270,
      color: "green"
    }
  ]);
  const drawableRef = useRef(null);

  const handleDraw = useCallback(async () => {
    if (!drawableRef.current) {
      return;
    }
    const drawable = drawableRef.current;
    const animation = drawable.animate(
      [
        { transform: "rotateZ(0)" },
        { transform: "rotateZ(90deg)" },
        { transform: "rotateZ(180deg)" },
        { transform: "rotateZ(270deg)" },
        { transform: "rotateZ(360deg)" }
      ],
      { duration: 1000, easing: "ease-in" }
    );
    await animation.finished;
    console.log("finished");
  }, []);

  return (
    <div className="App">
      <Drawable sectors={sectorsRef.current} ref={drawableRef} />
      <div>
        <button onClick={handleDraw}>Draw</button>
      </div>
    </div>
  );
}
