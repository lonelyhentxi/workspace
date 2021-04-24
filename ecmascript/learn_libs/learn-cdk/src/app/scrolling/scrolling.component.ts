import {ChangeDetectionStrategy, Component, OnInit} from '@angular/core';
import {ScrollDispatcher} from '@angular/cdk/overlay';
import {count, tap} from 'rxjs/operators';
import {Observable} from 'rxjs';

@Component({
  selector: 'app-scrolling',
  template: `
    <div>scrolling {{ scrollingTimes$ | async }} times</div>
    <cdk-virtual-scroll-viewport [itemSize]="18 * 7" style="height: 300px;" cdkScrollable>
      <h1 *cdkVirtualFor="let c of contents">{{ c }}</h1>
    </cdk-virtual-scroll-viewport>
  `
})
export class ScrollingComponent implements OnInit {

  contents: number[];
  scrollingTimes$?: Observable<number>;

  constructor(private scrollDispatcher: ScrollDispatcher) {
    this.contents = Array.from(new Array(100), (c, i) => i);
  }

  ngOnInit(): void {
    this.scrollingTimes$ = this.scrollDispatcher.scrolled().pipe(
      count(() => true),
      tap((i) => console.log(i))
    );
  }
}
