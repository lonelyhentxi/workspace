import React, {useRef,useEffect} from 'react';
import './LearnWebGL.css';
import { mat4 } from 'gl-matrix';

function loadShader(gl: WebGLRenderingContext,
     type: GLenum, source: string) : WebGLShader {
    const shader = gl.createShader(type)!;
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if(!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        const errorMessage = "An error occured compiling the shaders: " + gl.getShaderInfoLog(shader);
        alert(errorMessage);
        gl.deleteShader(shader);
        throw new Error(errorMessage);
    }
    return shader;
}

function initShaderProgram(gl: WebGLRenderingContext, vsSource: string, fsSource: string) {
    const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vsSource);
    const fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fsSource);
    const shaderProgram = gl.createProgram()!;
    gl.attachShader(shaderProgram, vertexShader);
    gl.attachShader(shaderProgram, fragmentShader);
    gl.linkProgram(shaderProgram);
    if(!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
        const errorMessage = 'Unable to initialize the shader program: ' + gl.getProgramInfoLog(shaderProgram);
        alert(errorMessage);
        throw new Error(errorMessage);
    }
    return shaderProgram;
}

function renderBasic(gl: WebGLRenderingContext, canvas: HTMLCanvasElement) {
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT);
}

interface ProgramInfo {
    program: WebGLProgram,
    attribLocations: {
        vertexPostion: number, 
    },
    uniformLocations: {
        projectionMatrix: WebGLUniformLocation,
        modelViewMatrix: WebGLUniformLocation, 
    },
}

interface WebGLBuffers {
    position: WebGLBuffer
}

function renderCube(gl: WebGLRenderingContext) {
    const shaderProgram = initShaderProgram(gl, 
    `
    attribute vec4 aVertexPostion;

    uniform mat4 uModelViewMatrix;
    uniform mat4 uProjectionMatrix;

    void main() {
        gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPostion;
    }
    `, 
    `
    void main() {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
    `);
    const programInfo:  ProgramInfo = {
        program: shaderProgram,
        attribLocations: {
            vertexPostion: gl.getAttribLocation(shaderProgram, 'aVertexPosition'),
        },
        uniformLocations: {
            projectionMatrix: gl.getUniformLocation(shaderProgram, 'uProjectionMatrix')!,
            modelViewMatrix: gl.getUniformLocation(shaderProgram, 'uModelViewMatrix')!,
        }
    };
    const initBuffers = () => {
        const positonBuffer = gl.createBuffer()!;
        gl.bindBuffer(gl.ARRAY_BUFFER, positonBuffer);
        const vertices = [
            1.0,  1.0,  0.0,
            -1.0, 1.0,  0.0,
            1.0,  -1.0, 0.0,
            -1.0, -1.0, 0.0
        ];
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
        return {
            position: positonBuffer,
        };
    };
    const drawScene = (
        gl: WebGLRenderingContext, 
        programInfo: ProgramInfo,
        buffers: WebGLBuffers, 
        ) => {
            gl.clearColor(0.0, 0.0, 0.0, 1.0);
            gl.clearDepth(1.0);
            gl.enable(gl.DEPTH_TEST);
            gl.depthFunc(gl.LEQUAL);
            gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
            const fieldOfView = 45 * Math.PI / 180;
            const aspect = (gl.canvas as HTMLCanvasElement).clientWidth
                / (gl.canvas as HTMLCanvasElement).clientHeight;
            const zNear = 0.1;
            const zFar = 100.0;
            const projectionMatrix = mat4.create();
            mat4.perspective(projectionMatrix, fieldOfView, aspect, zNear,zFar);
            const modelViewMatrix = mat4.create();
            mat4.translate(modelViewMatrix, modelViewMatrix, [-0.0, -0.0, -6.0]);
            {
                const numComponents = 3;
                const type = gl.FLOAT;
                const normalize = false;
                const stride = 0;
                const offset = 0;
                gl.bindBuffer(gl.ARRAY_BUFFER, buffers.position);
                gl.vertexAttribPointer(
                    programInfo.attribLocations.vertexPostion,
                    numComponents, type, normalize, stride, offset
                );
                gl.enableVertexAttribArray(programInfo.attribLocations.vertexPostion);
            }
            gl.useProgram(programInfo.program);
            gl.uniformMatrix4fv(
                programInfo.uniformLocations.projectionMatrix,
                false,
                projectionMatrix,
            );
            gl.uniformMatrix4fv(
                programInfo.uniformLocations.modelViewMatrix,
                false,
                modelViewMatrix
            );
            {
                const offset = 0;
                const vertexCount = 4;
                gl.drawArrays(gl.TRIANGLE_STRIP, offset, vertexCount);
            }
        };
        drawScene(gl, programInfo, initBuffers());
}

function LearnWebGL() {
    const webGLPainters = [
        renderBasic, renderCube
    ];
    const webGLCanvasRefs = useRef<HTMLCanvasElement[]>(new Array(webGLPainters.length));
    const webGLPainterSizes = new Array(webGLPainters.length).fill([640, 480]);
    webGLPainters.reverse();
    webGLPainterSizes.reverse();
    useEffect(()=>{
        const tasks = webGLPainters.map((p,i)=>()=>{
            const canvas = webGLCanvasRefs.current[i]!;
            return p(canvas.getContext("webgl")!, canvas);
        });
        tasks.reverse();
        tasks.forEach(t=>t());
    });
    return (
        <div className="LearnWebGL">
            <section  className="LearnWebGL-gallery">
                {
                    webGLPainters.map((_,i)=>(
                        <canvas className="LearnWebGL-canvas"
                            key={i}
                            ref={el=>webGLCanvasRefs.current[i]=el!}
                            width = {webGLPainterSizes[i][0]}
                            height = {webGLPainterSizes[i][1]}
                        >
                            canvas size: {webGLPainterSizes[i][0]} x {webGLPainterSizes[i][1]}
                        </canvas>
                    ))
                }
            </section>
        </div>
    );
}

export default LearnWebGL;