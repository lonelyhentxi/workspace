import { Component, OnInit } from '@angular/core';
import * as Rx from 'rxjs/Rx';

@Component({
  selector: 'app-rxjs-ch1',
  templateUrl: './rxjs-ch1.component.html',
  styleUrls: ['./rxjs-ch1.component.less']
})
export class RxjsCh1Component implements OnInit {
  holdTime:number = 0;
  rank:number = 0;

  constructor() {}

  ngOnInit() {
    const holdMeButton = document.querySelector('#hold-me');
    const mouseDown$ = Rx.Observable.fromEvent(holdMeButton, 'mousedown');
    const mouseUp$ = Rx.Observable.fromEvent(holdMeButton, 'mouseup');
    const holdTime$ = mouseUp$.timestamp().withLatestFrom(mouseDown$.timestamp()
      , (mouseUpEvent, mouseDownEvent) => {
        return mouseUpEvent.timestamp - mouseDownEvent.timestamp;
      });
    holdTime$.subscribe(ms=>{
      this.holdTime = ms;
    })
    holdTime$.flatMap(ms=>Rx.Observable.ajax('https://timing-sense-score-board.herokuapp.com/score/'+ms))
      .map(e=>e.response)
      .subscribe(res=>{
        this.rank = res.rank;
      })
  }

}
