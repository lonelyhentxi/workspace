import {Component, OnInit} from '@angular/core';
import {fromEvent, Observable, merge, Subject} from 'rxjs';
import {concatMap, filter, groupBy, map, mergeAll, pluck, takeUntil, throttleTime} from 'rxjs/operators';

@Component({
  selector: 'app-rxjs-ch8',
  templateUrl: './rxjs-ch8.component.html',
  styleUrls: ['./rxjs-ch8.component.less']
})
export class RxjsCh8Component implements OnInit {
  clickedThing: string = '';
  fooTimes: number = 0;
  barTimes: number = 0;

  constructor() {
  }

  ngOnInit() {
    const me = this;
    const click$ = new Subject();
    fromEvent(document, 'click').subscribe(click$);
    const clickedTagName$ = click$.pipe(pluck('target', 'tagName'), throttleTime(300))
      .subscribe((name: string) => {
        me.clickedThing = name;
      });
    const dragBox: HTMLElement = document.querySelector('#drag-me');
    const mouseDown$ = fromEvent(dragBox, 'mousedown');
    const mouseUp$ = fromEvent(dragBox, 'mouseup');
    const mouseOut$ = fromEvent(dragBox, 'mouseout');
    const mouseMove$ = fromEvent(dragBox, 'mousemove');
    const drag$ = mouseDown$.pipe(concatMap((startEvent: MouseEvent) => {
      const initialLeft = dragBox.offsetLeft;
      const initialTop = dragBox.offsetTop;
      const stop$ = merge(mouseOut$, mouseUp$);
      return mouseMove$.pipe(takeUntil(stop$)).pipe(map((moveEvent: MouseEvent) => {
        return {
          x: moveEvent.x - startEvent.x + initialLeft,
          y: moveEvent.y - startEvent.y + initialTop,
        };
      }));
    }));
    drag$.subscribe(event => {
      dragBox.style.left = event.x + 'px';
      dragBox.style.top = event.y + 'px';
    });
    const groupByClass$ = click$.pipe(groupBy((event: MouseEvent) => (event.target as HTMLButtonElement).className));
    groupByClass$.pipe(filter(value => value.key === 'foo'), mergeAll()).subscribe(() => (this.fooTimes++));
    groupByClass$.pipe(filter(value => value.key === 'bar'), mergeAll()).subscribe(() => (this.barTimes++));
  }
}
