import {Universe, Cell} from "@lonelyhentai/wasm-game-of-life";
import {memory} from "@lonelyhentai/wasm-game-of-life/wasm_game_of_life_bg";

const CELL_SIZE = 5;
const GRID_COLOR = "#ccc";
const DEAD_COLOR = "#FFF";
const ALIVE_COLOR = "#000";
let animationId = null;

const isPaused = () => {
    return animationId === null;
};

const playPauseButton = document.getElementById("play-pause");

const play = () => {
    playPauseButton.textContent = "||";
    canvasRenderLoop();
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
    for (let row = 0; row < height; row++) {
        for (let col = 0; col < width; col++) {
            const idx = getIndex(row, col);
            ctx.fillStyle = bitIsSet(idx, cells) ? ALIVE_COLOR : DEAD_COLOR;
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

const canvasRenderLoop = () => {
    drawGrid();
    drawCells();
    universe.tick();
    animationId = requestAnimationFrame(canvasRenderLoop);
};

play();
