"use strict";
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
const worker_threads_1 = require("worker_threads");
const path = __importStar(require("path"));
const events_1 = require("events");
const myWorker = new worker_threads_1.Worker(path.join(__dirname, 'worker_prove_console_log_async.js'));
const anotherWorker = new worker_threads_1.Worker(path.join(__dirname, 'worker_via_stream_log_sync.js'));
const masterEmitter = new events_1.EventEmitter();
let childWorkerExitCounter = 0;
myWorker.on('exit', () => {
    masterEmitter.emit('worker_count');
});
anotherWorker.on('exit', () => {
    masterEmitter.emit('worker_count');
});
masterEmitter.on('worker_count', () => {
    childWorkerExitCounter += 1;
    if (childWorkerExitCounter >= 4) {
        process.exitCode = 0;
    }
});
myWorker.on('message', (e) => {
    console.log('[master]: get message from worker1.');
    console.log(`[master]: hey say - ${e}`);
    masterEmitter.emit('worker_count');
});
anotherWorker.on('message', (e) => {
    console.log('[master]: get message from worker2.');
    console.log(`[master]: hey say - ${e}`);
    masterEmitter.emit('worker_count');
});
console.log('[master]: me, master, send message to worker1.');
myWorker.postMessage('this is master');
console.log('[master]: me, master, send message to worker2.');
anotherWorker.postMessage('this is master');
//# sourceMappingURL=master.js.map