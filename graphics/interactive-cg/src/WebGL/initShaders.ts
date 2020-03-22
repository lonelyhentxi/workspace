export function initShaders(gl: WebGLRenderingContext, vertexShaderText: string, fragmentShaderText: string): WebGLProgram
{
    const vertShader = gl.createShader(gl.VERTEX_SHADER)!;
    gl.shaderSource(vertShader, vertexShaderText);
    gl.compileShader(vertShader);
    if (!gl.getShaderParameter(vertShader, gl.COMPILE_STATUS)) {
        const msg = `Vertex shader failed to compile.  The error log is:`
            + `<pre>${gl.getShaderInfoLog(vertShader)}</pre>`;
        alert(msg);
        return -1;
    }

    const fragShader = gl.createShader(gl.FRAGMENT_SHADER)!;
    gl.shaderSource(fragShader, fragmentShaderText);
    gl.compileShader(fragShader);
    if (!gl.getShaderParameter(fragShader, gl.COMPILE_STATUS)) {
        const msg = "Fragment shader failed to compile.  The error log is:"
            + `<pre>${gl.getShaderInfoLog(fragShader)}</pre>`;
        alert(msg);
        return -1;
    }

    const program = gl.createProgram()!;
    gl.attachShader(program, vertShader);
    gl.attachShader(program, fragShader);
    gl.linkProgram(program);

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        const msg = "Shader program failed to link.  The error log is:"
            + `<pre>${gl.getProgramInfoLog(program)}</pre>`;
        alert(msg);
        return -1;
    }

    return program;
}
