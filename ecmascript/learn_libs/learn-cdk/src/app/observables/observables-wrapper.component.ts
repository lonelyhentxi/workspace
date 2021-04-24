import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-observables-wrapper',
  template: `
    <div>状态变更，第{{count}}次</div>
    <div class="content-wrapper" (cdkObserveContent)="projectContentChanged($event)" debounce="1000">
      <ng-content></ng-content>
    </div>
  `,
  styles: [
  ]
})
export class ObservablesWrapperComponent implements OnInit {
  count = 0;
  constructor() { }

  ngOnInit(): void {}

  projectContentChanged($event: MutationRecord[]) {
    ++this.count;
    console.log($event, this.count);
  }
}
