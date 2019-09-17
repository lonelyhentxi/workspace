import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-highlight-demo',
  template: `
    <p [appHighlight]="'yellow'">this should be yellow when mouse on hover</p>
  `,
})
export class HighlightDemoComponent implements OnInit {

  constructor() { }

  ngOnInit() {
  }

}
