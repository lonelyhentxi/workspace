import {Worker} from 'worker_threads'
import * as path from 'path';
import { EventEmitter} from 'events';

const myWorker = new Worker(path.join(__dirname, 'worker_prove_console_log_async.js'));
const anotherWorker = new Worker(path.join(__dirname, 'worker_via_stream_log_sync.js'));
const masterEmitter = new EventEmitter();
let childWorkerExitCounter = 0;

myWorker.on('exit', () => {
    masterEmitter.emit('worker_count');
});
anotherWorker.on('exit',()=>{
    masterEmitter.emit('worker_count');
});
masterEmitter.on('worker_count',()=>{
    childWorkerExitCounter+=1;
    if(childWorkerExitCounter>=4) {
        process.exitCode = 0;
    }
});

myWorker.on('message', (e: MessageEvent) => {
    console.log('[master]: get message from worker1.');
    console.log(`[master]: hey say - ${e}`);
    masterEmitter.emit('worker_count');
});

anotherWorker.on('message',(e: MessageEvent)=>{
    console.log('[master]: get message from worker2.');
    console.log(`[master]: hey say - ${e}`);
    masterEmitter.emit('worker_count');

});

console.log('[master]: me, master, send message to worker1.');
myWorker.postMessage('this is master');


console.log('[master]: me, master, send message to worker2.');
anotherWorker.postMessage('this is master');





