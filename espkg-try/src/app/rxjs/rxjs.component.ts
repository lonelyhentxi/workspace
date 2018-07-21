import { Component, OnInit } from '@angular/core';
import Rx from 'rxjs/Rx';

@Component({
  selector: 'app-rxjs',
  templateUrl: './rxjs.component.html',
  styleUrls: ['./rxjs.component.less']
})
export class RxjsComponent implements OnInit {

  constructor() { }

  ngOnInit() {
    const holdMeButton = document.querySelector('#hold-mde');
    const mouseDown$ = Rx.Observable.fromEvent(holdMeButton,'mousedown');
    const mouseUp$ = Rx.Observable.fromEvent(holdMeButton,'mouseup');
    const holdTime$ = mouseUp$.timestamp().withLatest
  }
}
