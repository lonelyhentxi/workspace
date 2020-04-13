import {add, flatten, scale, vec2, Vec2} from 'WebGL/MV';
import {initShaders} from "WebGL/initShaders";

export async function init(gl: WebGLRenderingContext, points: Vec2[], numPoints: number) {
    const vertices = [
        vec2(-1,-1),
        vec2(0, 1),
        vec2(1, -1)
    ];
    const u = add(vertices[0], vertices[1]);
    const v = add(vertices[0], vertices[2]);
    const p = scale(0.5, add(u, v));
    points.splice(0, points.length, p);
    for(let i=0; i< numPoints;i++) {
        const j = Math.floor(Math.random() * 3);
        let newPoint = add(points[i], vertices[j]);
        newPoint = scale(0.5, newPoint);
        points.push(newPoint);
    }
    const vsRes = await fetch(process.env.PUBLIC_URL + "vertex-shader.glsl");
    const fsRes = await fetch(process.env.PUBLIC_URL + 'fragment-shader.glsl');
    gl.viewport(0, 0, gl.canvas.width,gl.canvas.height);
    gl.clearColor(1.,1.,1.,1.);
    const program = initShaders(gl, await vsRes.text(), await fsRes.text());
    gl.useProgram(program);
    const bufferId = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, bufferId);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(points), gl.STATIC_DRAW);
    const vPosition = gl.getAttribLocation(program, 'vPosition');
    gl.vertexAttribPointer(vPosition, 2, gl.FLOAT, false,0,0);
    gl.enableVertexAttribArray(vPosition);
    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.drawArrays(gl.POINTS, 0, points.length);
}
