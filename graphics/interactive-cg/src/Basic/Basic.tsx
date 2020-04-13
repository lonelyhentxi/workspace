import React from "react";
import {init} from "./sierpinski";
import {Vec2} from "WebGL/MV";

export class Basic extends React.Component {
    points: Vec2[] = [];

    async componentDidMount() {
        const canvas = document.querySelector('#basic-canvas')! as HTMLCanvasElement;
        const gl = canvas.getContext('webgl') as WebGLRenderingContext;
        await init(gl, this.points, 5000);
    }

    render() {
        return (<div>
            <canvas id="basic-canvas" width="512" height="512">
                Your browser doesn't appear to support the HTML5 <code>&lt;canvas&gt;</code> element.
            </canvas>
        </div>)
    }
};
