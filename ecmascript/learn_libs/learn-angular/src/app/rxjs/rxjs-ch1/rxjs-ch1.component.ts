import {Component, OnInit} from '@angular/core';
import {fromEvent, Observable} from 'rxjs';
import {flatMap, map, timestamp, withLatestFrom} from 'rxjs/operators';
import {ajax} from 'rxjs/ajax';

@Component({
  selector: 'app-rxjs-ch1',
  templateUrl: './rxjs-ch1.component.html',
  styleUrls: ['./rxjs-ch1.component.less']
})
export class RxjsCh1Component implements OnInit {
  holdTime: number = 0;
  rank: number = 0;

  constructor() {
  }

  ngOnInit() {
    const holdMeButton = document.querySelector('#hold-me');
    const mouseDown$ = fromEvent(holdMeButton, 'mousedown');
    const mouseUp$ = fromEvent(holdMeButton, 'mouseup');
    const holdTime$ = mouseUp$.pipe(timestamp(), withLatestFrom(mouseDown$.pipe(timestamp())
      , (mouseUpEvent, mouseDownEvent) => {
        return mouseUpEvent.timestamp - mouseDownEvent.timestamp;
      }));
    holdTime$.subscribe(ms => {
      this.holdTime = ms;
    });
    holdTime$.pipe(flatMap(ms => ajax('https://timing-sense-score-board.herokuapp.com/score/' + ms)),
      map((e: any) => e.response)
    ).subscribe(res => {
      this.rank = res.rank;
    });
  }

}
