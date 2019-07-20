import { parentPort,isMainThread, MessagePort } from 'worker_threads';
import * as fs from 'fs';

if(!isMainThread) {
    const checkedParentPort = parentPort as MessagePort;
    checkedParentPort.once('message',(e: MessageEvent) => {
        fs.writeSync(1,'[worker2]: master say what?\n');
        fs.writeSync(1,`[worker2]: hey say - ${e}\n`);
        fs.writeSync(1,'[worker2]: holly\n');
        checkedParentPort.postMessage('hey, here is worker2, get that message.');
        process.exitCode = 0;
    });
}