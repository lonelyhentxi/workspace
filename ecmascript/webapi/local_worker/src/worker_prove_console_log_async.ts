import { parentPort,isMainThread, MessagePort } from 'worker_threads';

if(!isMainThread) {
    const checkedParentPort = parentPort as MessagePort;
    checkedParentPort.once('message',(e: MessageEvent) => {
        console.log('[worker1]: master say what?');
        console.log(`[worker1]: he say - ${e}`); // async in windows
        console.log('[worker1]: holly'); // async in windows
        checkedParentPort.postMessage('hey, here is worker1, get that message.');
        process.exitCode = 0;
    });
}