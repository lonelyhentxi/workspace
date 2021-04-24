import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-observables',
  template: `
    <app-observables-wrapper >
      <app-observables-content ></app-observables-content>
    </app-observables-wrapper>
  `,
  styles: [
  ]
})
export class ObservablesComponent implements OnInit {
  constructor() { }

  ngOnInit(): void {}
}
