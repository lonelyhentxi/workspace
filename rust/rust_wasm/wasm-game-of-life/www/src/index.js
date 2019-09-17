import {Universe, Cell} from "@lonelyhentai/wasm-game-of-life";
import {memory} from "@lonelyhentai/wasm-game-of-life/wasm_game_of_life_bg";

const CELL_SIZE = 5;
const GRID_COLOR = "#ccc";
const DEAD_COLOR = "#FFF";
const ALIVE_COLOR = "#000";

const canvas = document.getElementById("game-of-life-canvas");

const universe = Universe.new();
const width = universe.width();
const height = universe.height();
canvas.height = (CELL_SIZE + 1) * height + 1;
canvas.width = (CELL_SIZE + 1) * width + 1;
const ctx = canvas.getContext('2d');
const getIndex = (row, column) => {
    return row * width + column;
};
const bitIsSet = (n,arr) => {
    const byte = Math.floor(n/8);
    const mask = 1 << (n%8);
    return (arr[byte] & mask) === mask;
};
const drawGrid = () => {
    ctx.beginPath();
    ctx.strokeStyle = GRID_COLOR;
    for (let i = 0; i <= width; i++) {
        ctx.moveTo(i * (CELL_SIZE + 1) + 1, 0);
        ctx.lineTo(i * (CELL_SIZE + 1) + 1, (CELL_SIZE + 1) * height + 1);
    }
    for (let j = 0; j <= height; j++) {
        ctx.moveTo(0, j * (CELL_SIZE + 1) + 1);
        ctx.lineTo((CELL_SIZE + 1) * width + 1, j * (CELL_SIZE + 1) + 1);
    }
    ctx.stroke();
};

const drawCells = () => {
    const cellsPtr = universe.cells();
    const cells = new Uint8Array(memory.buffer, cellsPtr, width * height/8);
    ctx.beginPath();
    ctx.fillStyle = ALIVE_COLOR;
    for (let row = 0; row < height; row++) {
        for (let col = 0; col < width; col++) {
            const idx = getIndex(row, col);
            if(!bitIsSet(idx,cells)) {
                continue;
            }
            ctx.fillRect(col * (CELL_SIZE + 1) + 1, row * (CELL_SIZE + 1) + 1, CELL_SIZE, CELL_SIZE);
        }
    }
    ctx.fillStyle = DEAD_COLOR;
    for (let row = 0; row < height; row++) {
        for (let col = 0; col < width; col++) {
            const idx = getIndex(row, col);
            if(bitIsSet(idx,cells)) {
                continue;
            }
            ctx.fillRect(col * (CELL_SIZE + 1) + 1, row * (CELL_SIZE + 1) + 1, CELL_SIZE, CELL_SIZE);
        }
    }
    ctx.stroke();
};

canvas.addEventListener("click",ev => {
    const boundingRect = canvas.getBoundingClientRect();
    const scaleX = canvas.width / boundingRect.width;
    const scaleY = canvas.height / boundingRect.height;
    const canvasLeft = (ev.clientX - boundingRect.left) * scaleX;
    const canvasTop = (ev.clientY - boundingRect.top) * scaleY;
    const row = Math.min(Math.floor(canvasTop / (CELL_SIZE+1)),height-1);
    const col = Math.min(Math.floor(canvasLeft / (CELL_SIZE+1)), width-1);
    universe.toggle_cell(row,col);
    drawGrid();
    drawCells();
});

class Fps {
    constructor() {
        this.fps = document.getElementById("fps");
        this.frames = [];
        this.lastFrameTimeStamp = performance.now();
    }
    render() {
        const now = performance.now();
        const delta = now - this.lastFrameTimeStamp;
        this.lastFrameTimeStamp = now;
        const fps = 1/delta*1000;
        this.frames.push(fps);
        if(this.frames.length>100) {
            this.frames.shift();
        }
        let min = Infinity;
        let max = -Infinity;
        let sum = 0;
        for(let i=0;i<this.frames.length;i++) {
            sum += this.frames[i];
            min = Math.min(this.frames[i], min);
            max = Math.max(this.frames[i], max);
        }
        let mean = sum / this.frames.length;
        this.fps.textContent = `
Frames per Second:
         latest = ${Math.round(fps)}
avg of last 100 = ${Math.round(mean)}
min of last 100 = ${Math.round(min)}
max of last 100 = ${Math.round(max)}
            `.trim();
    }
}

const fps = new Fps();

let animationId = null;

const isPaused = () => {
    return animationId === null;
};

const playPauseButton = document.getElementById("play-pause");

const canvasRenderInit = () => {
    fps.render();
    drawGrid();
    drawCells();
    universe.tick();
    animationId = requestAnimationFrame(canvasRenderLoop);
};

const canvasRenderLoop = () => {
    fps.render();
    drawCells();
    universe.tick();
    animationId = requestAnimationFrame(canvasRenderLoop);
};

const play = () => {
    playPauseButton.textContent = "||";
    canvasRenderInit();
};

const pause = () => {
    playPauseButton.textContent = "▶";
    cancelAnimationFrame(animationId);
    animationId = null;
};

playPauseButton.addEventListener("click", ev => {
    if(isPaused()) {
        play();
    } else {
        pause();
    }
});

play();
