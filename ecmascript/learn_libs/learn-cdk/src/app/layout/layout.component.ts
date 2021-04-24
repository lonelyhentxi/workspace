import { Component, OnInit } from '@angular/core';
import {BreakpointObserver, Breakpoints} from '@angular/cdk/layout';
import {map, tap} from 'rxjs/operators';
import {Observable} from 'rxjs';

@Component({
  selector: 'app-layout',
  template: `
    <div style="text-align: center">
      <div>This screen {{isSmallWidth?'is':'is not'}} small width on init</div>
      <div>The orientation of this device is {{ this.orientation$ | async }}</div>
      <div>Your device type may be {{ deviceType$ | async }}</div>
    </div>
  `,
  styles: [
  ]
})
export class LayoutComponent implements OnInit {

  isSmallWidth = false;
  orientation$: Observable<string>;
  deviceType$: Observable<string>;

  constructor(private breakPointObserver: BreakpointObserver) {
    this.orientation$ = breakPointObserver.observe('(orientation: portrait)').pipe(
      map(r => r.matches ? 'portrait' : 'landscape')
    );
    this.deviceType$ = breakPointObserver.observe([Breakpoints.Handset, Breakpoints.Tablet, Breakpoints.Web]).pipe(
      map(result => Object.entries(result.breakpoints).findIndex(b => b[1])),
      map( i => i >= 0 ? ['handset', 'tablet', 'web'][i] : 'unknown')
    );
  }

  ngOnInit(): void {
    this.isSmallWidth = this.breakPointObserver.isMatched(`(max-width: 640px)`);
  }
}
