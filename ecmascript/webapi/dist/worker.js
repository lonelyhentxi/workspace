"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const worker_threads_1 = require("worker_threads");
if (!worker_threads_1.isMainThread) {
    const checkedParentPort = worker_threads_1.parentPort;
    checkedParentPort.once('message', (e) => {
        if (process.stdout._handle) {
            process.stdout._handle.setBlocking(true);
        }
        console.log('master say what?');
        console.log(e);
        console.log('holly');
        checkedParentPort.postMessage('hey, here is worker, get that message.');
        process.exit();
    });
}
//# sourceMappingURL=worker.js.map