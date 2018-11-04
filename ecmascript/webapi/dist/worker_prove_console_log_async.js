"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const worker_threads_1 = require("worker_threads");
if (!worker_threads_1.isMainThread) {
    const checkedParentPort = worker_threads_1.parentPort;
    checkedParentPort.once('message', (e) => {
        console.log('[worker1]: master say what?');
        console.log(`[worker1]: he say - ${e}`); // async in windows
        console.log('[worker1]: holly'); // async in windows
        checkedParentPort.postMessage('hey, here is worker1, get that message.');
        process.exitCode = 0;
    });
}
//# sourceMappingURL=worker_prove_console_log_async.js.map