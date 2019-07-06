import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'app-web-worker-demo',
  template: `
    <div><input type="text" #input (focusout)="processTextByWorker(input.value)"></div>
    <p>{{processedText}}</p>
  `,
  styles: []
})
export class WebWorkerDemoComponent implements OnInit {
  processedText: string = '';
  worker: Worker;

  processTextByWorker(text: string) {
    if(this.worker){
      this.worker.postMessage(text);
    }
  }

  constructor() {
    if (typeof Worker !== 'undefined') {
      const worker = new Worker('./demo.worker', {type: 'module'});
      worker.onmessage = ({data}) => {
        this.processedText = data;
      };
      this.worker = worker;
    } else {
      this.processedText = 'your client do not support web worker';
    }
  }

  ngOnInit() {
  }

}
