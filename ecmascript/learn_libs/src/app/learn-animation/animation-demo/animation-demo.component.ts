import {Component, OnInit} from '@angular/core';
import {trigger, state, style, animate, transition} from '@angular/animations';

@Component({
  selector: 'app-animation-demo',
  template: `
    <div [@openClose]="isOpen ? 'open' : 'closed'" class="open-close-container">
      <span>The box is now {{ isOpen ? 'Open' : 'Closed' }}! </span>
      <button type="button" class="btn btn-primary" (click)="toggle()">Toggle State</button>
    </div>
  `,
  styles: [`
    @import "https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css";

    :host {
      display: block;
    }

    .open-close-container {
      border: 1px solid #dddddd;
      margin: 20px auto;
      width: fit-content;
      padding: 20px;
      color: #000000;
      font-weight: bold;
      font-size: 20px;
      border-radius: 4px;
    }
  `],
  animations: [
    trigger('openClose', [
      state('open', style({
        opacity: 1,
        backgroundColor: 'aqua'
      })),
      state('closed', style({
        opacity: 0.5,
        background: 'green'
      })),
      transition('open => closed', [
        animate('1s')
      ]),
      transition('closed => open', [
        animate('0.5s')
      ])
    ])
  ]
})
export class AnimationDemoComponent implements OnInit {
  isOpen = true;

  constructor() {
  }

  ngOnInit() {
  }

  toggle() {
    this.isOpen = !this.isOpen;
  }
}
