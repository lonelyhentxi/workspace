import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'app-observables-content',
  template: `
    <p>
      <button mat-button (click)="toggleColor()" [ngStyle]="{backgroundColor: color}">Toggle Color</button>
    </p>
  `,
  styles: []
})
export class ObservablesContentComponent implements OnInit {

  color = '';

  constructor() {
  }


  private static randomColorChannel() {
    return Math.round(Math.random() * 255);
  }

  ngOnInit(): void {
    this.toggleColor();
  }

  toggleColor() {
    this.color = `rgb(${Array.from(new Array(3), ObservablesContentComponent.randomColorChannel).join(',')}`;
  }
}
