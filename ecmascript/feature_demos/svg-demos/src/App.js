import React from "react";
import "./styles.css";

export default function App() {
  return (
    <div className="App">
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 100 100"
        className="icon"
      >
        <g>
          <circle
            cx="50"
            cy="50"
            r="44"
            strokeWidth="6"
            stroke="currentColor"
            fill="transparent"
          />
          <line
            x1="30"
            y1="30"
            x2="70"
            y2="70"
            stroke="currentColor"
            strokeWidth="6"
          />
          <line
            x1="70"
            y1="30"
            x2="30"
            y2="70"
            stroke="currentColor"
            strokeWidth="6"
          />
        </g>
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 100 100"
        className="icon"
      >
        <g>
          <polyline
            points="25 10, 70 50, 25 90"
            fill="transparent"
            stroke="currentColor"
            strokeWidth="6"
          />
        </g>
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 100 100"
        className="icon"
      >
        <g>
          <line
            x1="25"
            y1="25"
            x2="75"
            y2="75"
            stroke="currentColor"
            strokeWidth="6"
          />
          <line
            x1="75"
            y1="25"
            x2="25"
            y2="75"
            stroke="currentColor"
            strokeWidth="6"
          />
        </g>
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 100 100"
        className="panel"
      >
        <path d="M 10 10 H 90 V 90 H 10 Z" fill="transparent" stroke="black" />
        <path
          d="M10 10 C 20 20, 40 40, 50 10"
          stroke="black"
          fill="transparent"
        />
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 200 200"
        className="panel"
      >
        <path
          d="M10 80 C 40 10, 65 10, 95 80 S 150 150, 180 80"
          stroke="black"
          fill="transparent"
        />
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 200 200"
        className="panel"
      >
        <path
          d="M10 80 Q 52.5 10, 95 80 T 180 80"
          stroke="black"
          fill="transparent"
        />
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 320 320"
        className="panel"
      >
        <path
          d="M10 315 L 110 215 A 30 50 -45 0 1 215.1 109.9 L 315 10"
          stroke="black"
          fill="green"
          strokeWidth="2"
          fillOpacity="0.5"
        />
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 160 160"
        className="panel"
      >
        <line
          x1="40"
          x2="120"
          y1="20"
          y2="20"
          stroke="black"
          strokeWidth="20"
          strokeLinecap="butt"
        />
        <line
          x1="40"
          x2="120"
          y1="60"
          y2="60"
          stroke="black"
          strokeWidth="20"
          strokeLinecap="square"
        />
        <line
          x1="40"
          x2="120"
          y1="100"
          y2="100"
          stroke="black"
          strokeWidth="20"
          strokeLinecap="round"
        />
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 300 300"
        className="panel"
      >
        <polyline
          points="40 60 80 20 120 60"
          stroke="black"
          strokeWidth="20"
          strokeLinecap="butt"
          fill="none"
          strokeLinejoin="miter"
        />

        <polyline
          points="40 140 80 100 120 140"
          stroke="black"
          strokeWidth="20"
          strokeLinecap="round"
          fill="none"
          strokeLinejoin="round"
        />

        <polyline
          points="40 220 80 180 120 220"
          stroke="black"
          strokeWidth="20"
          strokeLinecap="square"
          fill="none"
          strokeLinejoin="bevel"
        />
      </svg>
      <svg
        className="panel"
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 200 200"
      >
        <path
          d="M 10 75 Q 50 10 100 75 T 190 75"
          stroke="black"
          strokeLinecap="round"
          strokeDasharray="5,10,5"
          fill="none"
        />
        <path
          d="M 10 75 L 190 75"
          stroke="red"
          strokeLinecap="round"
          strokeWidth="1"
          strokeDasharray="5,5"
          fill="none"
        />
      </svg>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 240 240"
        className="panel"
      >
        <defs>
          <linearGradient id="Gradient1">
            <stop className="stop1" offset="0%" />
            <stop className="stop2" offset="50%" />
            <stop className="stop3" offset="100%" />
          </linearGradient>
          <linearGradient id="Gradient2" x1="0" x2="0" y1="0" y2="1">
            <stop offset="0%" stopColor="red" />
            <stop offset="50%" stopColor="black" stopOpacity="0" />
            <stop offset="100%" stopColor="blue" />
          </linearGradient>
          <style type="text/css">
            {`#rect1 { fill: url(#Gradient1); }
            .stop1 { stop-color: red; }
            .stop2 { stop-color: black; stop-opacity: 0; }
            .stop3 { stop-color: blue; }`}
          </style>
        </defs>
        <rect
          id="rect1"
          x="10"
          y="10"
          rx="15"
          ry="15"
          width="100"
          height="100"
        />
        <rect
          x="10"
          y="120"
          rx="15"
          ry="15"
          width="100"
          height="100"
          fill="url(#Gradient2)"
        />
      </svg>
      <svg
        viewBox="0 0 120 120"
        className="panel"
        xmlns="http://www.w3.org/2000/svg"
      >
        <defs>
          <radialGradient
            id="Gradient"
            cx="0.5"
            cy="0.5"
            r="0.5"
            fx="0.25"
            fy="0.25"
          >
            <stop offset="0%" stopColor="red" />
            <stop offset="100%" stopColor="blue" />
          </radialGradient>
        </defs>

        <rect
          x="10"
          y="10"
          rx="15"
          ry="15"
          width="100"
          height="100"
          fill="url(#Gradient)"
          stroke="black"
          strokeWidth="2"
        />

        <circle
          cx="60"
          cy="60"
          r="50"
          fill="transparent"
          stroke="white"
          strokeWidth="2"
        />
        <circle cx="35" cy="35" r="2" fill="white" stroke="white" />
        <circle cx="60" cy="60" r="2" fill="white" stroke="white" />
        <text
          x="38"
          y="40"
          fill="white"
          fontFamily="sans-serif"
          fontSize="10pt"
        >
          (fx,fy)
        </text>
        <text
          x="63"
          y="63"
          fill="white"
          fontFamily="sans-serif"
          fontSize="10pt"
        >
          (cx,cy)
        </text>
      </svg>

      <svg
        viewBox="0 0 220 220"
        className="panel"
        xmlns="http://www.w3.org/2000/svg"
      >
        <defs>
          <radialGradient
            id="GradientPad"
            cx="0.5"
            cy="0.5"
            r="0.4"
            fx="0.75"
            fy="0.75"
            spreadMethod="pad"
          >
            <stop offset="0%" stopColor="red" />
            <stop offset="100%" stopColor="blue" />
          </radialGradient>
          <radialGradient
            id="GradientRepeat"
            cx="0.5"
            cy="0.5"
            r="0.4"
            fx="0.75"
            fy="0.75"
            spreadMethod="repeat"
          >
            <stop offset="0%" stopColor="red" />
            <stop offset="100%" stopColor="blue" />
          </radialGradient>
          <radialGradient
            id="GradientReflect"
            cx="0.5"
            cy="0.5"
            r="0.4"
            fx="0.75"
            fy="0.75"
            spreadMethod="reflect"
          >
            <stop offset="0%" stopColor="red" />
            <stop offset="100%" stopColor="blue" />
          </radialGradient>
        </defs>

        <rect
          x="10"
          y="10"
          rx="15"
          ry="15"
          width="100"
          height="100"
          fill="url(#GradientPad)"
        />
        <rect
          x="10"
          y="120"
          rx="15"
          ry="15"
          width="100"
          height="100"
          fill="url(#GradientRepeat)"
        />
        <rect
          x="120"
          y="120"
          rx="15"
          ry="15"
          width="100"
          height="100"
          fill="url(#GradientReflect)"
        />

        <text
          x="15"
          y="30"
          fill="white"
          fontFamily="sans-serif"
          fontSize="12pt"
        >
          Pad
        </text>
        <text
          x="15"
          y="140"
          fill="white"
          fontFamily="sans-serif"
          fontSize="12pt"
        >
          Repeat
        </text>
        <text
          x="125"
          y="140"
          fill="white"
          fontFamily="sans-serif"
          fontSize="12pt"
        >
          Reflect
        </text>
      </svg>
      <svg
        viewBox="0 0 600 300"
        xmlns="http://www.w3.org/2000/svg"
        style={{ width: 300, height: 150 }}
      >
        <defs>
          <linearGradient id="Gradient3">
            <stop offset="5%" stopColor="white" />
            <stop offset="95%" stopColor="blue" />
          </linearGradient>
          <linearGradient id="Gradient4" x1="0" x2="0" y1="0" y2="1">
            <stop offset="5%" stopColor="red" />
            <stop offset="95%" stopColor="orange" />
          </linearGradient>

          <pattern id="Pattern" x="0" y="0" width=".25" height=".25">
            <rect x="0" y="0" width="50" height="50" fill="skyblue" />
            <rect x="0" y="0" width="25" height="25" fill="url(#Gradient4)" />
            <circle
              cx="25"
              cy="25"
              r="20"
              fill="url(#Gradient3)"
              fillOpacity="0.5"
            />
          </pattern>
        </defs>

        <pattern
          id="Pattern1"
          width=".25"
          height=".25"
          patternContentUnits="objectBoundingBox"
        >
          <rect x="0" y="0" width=".25" height=".25" fill="skyblue" />
          <rect x="0" y="0" width=".125" height=".125" fill="url(#Gradient4)" />
          <circle
            cx=".125"
            cy=".125"
            r=".1"
            fill="url(#Gradient3)"
            fillOpacity="0.5"
          />
        </pattern>

        <pattern
          id="Pattern2"
          width="50"
          height="50"
          patternUnits="userSpaceOnUse"
        >
          <rect x="0" y="0" width="50" height="50" fill="skyblue" />
          <rect x="0" y="0" width="25" height="25" fill="url(#Gradient4)" />
          <circle
            cx="25"
            cy="25"
            r="20"
            fill="url(#Gradient3)"
            fillOpacity="0.5"
          />
        </pattern>

        <rect
          fill="url(#Pattern)"
          stroke="black"
          x="0"
          y="0"
          width="200"
          height="300"
        />

        <rect
          fill="url(#Pattern1)"
          stroke="black"
          x="200"
          y="0"
          width="200"
          height="300"
        />

        <rect
          fill="url(#Pattern2)"
          stroke="black"
          x="400"
          y="0"
          width="200"
          height="300"
        />
      </svg>
      <svg
        viewBox="0 0 200 200"
        style={{ width: 200, height: 200 }}
        xmlns="http://www.w3.org/2000/svg"
      >
        <text>
          <tspan fontWeight="bold" fill="red">
            This is bold and red
          </tspan>
        </text>
      </svg>
      <svg
        viewBox="0 0 200 50"
        style={{ width: "100%", height: "16px" }}
        xmlns="http://www.w3.org/2000/svg"
        preserveAspectRatio="none"
      >
        <path
          d="M 0 0 L 0 50 L 200 50 L 200 0 C 150 60, 50 60, 0 0"
          fill="white"
          stroke="black"
        />
      </svg>
      <svg
        viewBox="0 0 61 20"
        style={{ width: "244px", height: "80px", marginTop: "10px" }}
        xmlns="http://www.w3.org/2000/svg"
        preserveAspectRatio="none"
      >
        <defs>
          <linearGradient
            id="tarot-auto-sliding-list-goods-item-v2-price-background-layer1"
            x1="0%"
            y1="100%"
            x2="0%"
            y2="0%"
            gradientTransform="rotate(270)"
          >
            <stop offset="0%" stopColor="#F10023" />
            <stop offset="100%" stopColor="#FD6007" />
          </linearGradient>
          <linearGradient
            id="tarot-auto-sliding-list-goods-item-v2-price-background-layer2"
            x1="0%"
            y1="100%"
            x2="0%"
            y2="0%"
            gradientTransform="rotate(270)"
          >
            <stop offset="0%" stopColor="#FD0B2E" />
            <stop offset="100%" stopColor="#FD6007" />
          </linearGradient>
          <linearGradient
            id="tarot-auto-sliding-list-goods-item-v2-price-background-layer3"
            x1="0%"
            y1="100%"
            x2="0%"
            y2="0%"
            gradientTransform="rotate(270)"
          >
            <stop offset="0%" stopColor="#FF4600" />
            <stop offset="100%" stopColor="#FF9E1E" />
          </linearGradient>
        </defs>
        <path
          d="M 0 20 L 6 6 S 8 0, 14 0 L 61 0 L 61 20 Z"
          fill="url(#tarot-auto-sliding-list-goods-item-v2-price-background-layer1)"
        />
        <path
          d="M 0 20 L 6 6 S 8 0, 14 0 L 61 0 L 61 20 Z"
          fill="url(#tarot-auto-sliding-list-goods-item-v2-price-background-layer2)"
        />
        <path
          d="M 0 20 L 6 6 S 8 0, 14 0 L 61 0 L 61 20 Z"
          fill="url(#tarot-auto-sliding-list-goods-item-v2-price-background-layer3)"
        />
      </svg>

      <svg
        viewBox="0 0 72 16"
        style={{
          width: "288px",
          height: "64px",
          marginTop: "10px",
          marginLeft: "10px"
        }}
        xmlns="http://www.w3.org/2000/svg"
        preserveAspectRatio="none"
      >
        <defs>
          <linearGradient
            id="tarot-auto-sliding-list-goods-item-v2-tip-background-layer1"
            x1="0%"
            y1="100%"
            x2="0%"
            y2="0%"
            gradientTransform="rotate(270)"
          >
            <stop offset="0%" stopColor="#FFF6DF" />
            <stop offset="100%" stopColor="#FFEBBC" />
          </linearGradient>
        </defs>
        <path
          d="M 0 0 L 72 0 L 68 12 S 66 16, 64 16 L 0 16 Z"
          fill="url(#tarot-auto-sliding-list-goods-item-v2-tip-background-layer1)"
        />
      </svg>

      <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 100 100"
        className="icon"
      >
        <g>
          <circle cx="50" cy="50" r="50" fill="grey" />
          <polyline
            points="60 20, 30 50, 60 80"
            fill="transparent"
            stroke="white"
            strokeWidth="4"
          />
        </g>
      </svg>
    </div>
  );
}
