import React, { useEffect, useRef, useCallback, useState } from "react";
import "./styles.css";

export function useRAFThrottle() {
  const lock = useRef(new Map());
  const rafs = useRef(new Map());
  useEffect(() => {
    const rafMap = rafs.current;
    return () => {
      for (const v of rafMap.keys()) {
        v && cancelAnimationFrame(v);
      }
    };
  }, []);
  const rafThrottle = useCallback((event) => {
    const cancelEvent = () => {
      rafs.current.delete(event);
    };
    const wrapperedEvent = (...args) => {
      if (!lock.current.get(event)) {
        lock.current.set(event, true);
        rafs.current.set(
          event,
          requestAnimationFrame(() => {
            typeof event === "function" && event(...args);
            rafs.current.delete(event);
            lock.current.delete(event);
          })
        );
      }
    };
    return [wrapperedEvent, cancelEvent];
  }, []);
  return rafThrottle;
}

export default function App() {
  const rafThrottle = useRAFThrottle();
  const canvasRef = useRef(null);
  const [size, setSize] = useState([0, 0]);

  const draw = useCallback(() => {
    if (canvasRef.current) {
      /**
       * @type { HTMLCanvasElement }
       */
      const canvas = canvasRef.current;
      const ctx = canvas.getContext("2d");
      ctx.fillStyle = "red";
      ctx.fillRect(0, 0, canvas.width, canvas.height);
    }
  }, []);

  useEffect(() => {
    const [resizeCanvas, cancelEvent] = rafThrottle(() => {
      setSize([window.innerWidth / 2, window.innerHeight / 2]);
    });
    window.addEventListener("resize", resizeCanvas);
    return () =>
      window.removeEventListener("resize", resizeCanvas) && cancelEvent();
  }, [rafThrottle]);

  useEffect(() => {
    draw();
  }, [size, draw]);

  useEffect(() => {
    setSize([window.innerWidth / 2, window.innerHeight / 2]);
  }, []);

  return (
    <div className="App">
      <canvas ref={canvasRef} width={size[0]} height={size[1]} />
    </div>
  );
}
