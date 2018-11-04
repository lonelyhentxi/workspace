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
const fs = __importStar(require("fs"));
if (!worker_threads_1.isMainThread) {
    const checkedParentPort = worker_threads_1.parentPort;
    checkedParentPort.once('message', (e) => {
        fs.writeSync(1, '[worker2]: master say what?\n');
        fs.writeSync(1, `[worker2]: hey say - ${e}\n`);
        fs.writeSync(1, '[worker2]: holly\n');
        checkedParentPort.postMessage('hey, here is worker2, get that message.');
        process.exitCode = 0;
    });
}
//# sourceMappingURL=worker_via_stream_log_sync.js.map