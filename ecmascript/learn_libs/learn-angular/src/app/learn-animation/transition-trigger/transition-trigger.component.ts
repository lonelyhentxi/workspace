import {Component, HostBinding, OnInit} from '@angular/core';
import {animate, keyframes, state, style, transition, trigger} from '@angular/animations';

@Component({
  selector: 'app-transition-trigger',
  template: `
    <div [@openClose]="isOpen ? 'open' : 'closed'" class="open-close-container">
      <p *ngFor="let _ of boxes" @flyInOut>The box is now {{ isOpen ? 'Open' : 'Closed' }}! </p>
      <button type="button" class="btn btn-primary" (click)="isOpen=!isOpen">Toggle State</button>
      <button type="button" class="btn btn-primary" (click)="boxes.push({})">Add Box</button>
      <button type="button" class="btn btn-primary" (click)="boxes.splice(boxes.length-1,1)">Remove Box</button>
    </div>
    <div class="center-container">
      <p @myInsertRemoveTrigger *ngIf="isShow">The box is inserted</p>
      <button type="button" class="btn btn-primary" (click)="isShow=!isShow">toggle</button>
    </div>
    <div [@.disabled]="isDisabled" class="center-container">
      <div [@openClose]="isOpen?'open':'closed'"
           (@openClose.start)="onAnimationStart($event)"
           (@openClose.done)="onAnimationEnd($event)"
           class="open-close-container"
      >
        <p>The box is now {{ isOpen ? 'open' : 'closed' }} and {{ isDisabled ? 'disabled' : 'enabled'}}!</p>
        <button type="button" class="btn btn-primary" (click)="isDisabled=!isDisabled">Toggle Disable</button>
        <button type="button" class="btn btn-primary" (click)="isOpen=!isOpen">Toggle State</button>
        <button type="button" class="btn btn-primary" (click)="animationsDisabled=!animationsDisabled">Toggle All Disabled</button>
      </div>
    </div>
    <div class="center-container">
      <div [@keyframesDemo]="isActive?'active':'inactive'">Keyframes Demo</div>
      <button type="button" class="btn btn-primary" (click)="isActive=!isActive">Toggle Active</button>
    </div>
  `,
  styles: [`
    @import "https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css";
  `,
      `
      button {
        margin: auto 5px;
      }

      .center-container {
        width: fit-content;
        text-align: center;
        margin: 20px auto;
      }

      :host {
        display: block;
      }

      .open-close-container {
        border: 1px solid #dddddd;
        margin: 20px auto;
        width: fit-content;
        height: fit-content;
        padding: 20px;
        color: #000000;
        font-weight: bold;
        font-size: 20px;
        border-radius: 4px;
        text-align: center;
      }
    `],
  animations: [
    trigger('openClose', [
      state('open', style({
        opacity: 1,
        backgroundColor: 'yellow'
      })),
      state('closed', style({
        opacity: 0.5,
        backgroundColor: 'green'
      })),
      transition('open => closed', [
        animate('1s')
      ]),
      transition('closed => open', [
        animate('0.5s')
      ]),
      transition('* => closed', [
        animate('1s')
      ]),
      transition('* => open', [
        animate('0.5s')
      ]),
      transition('open <=> closed', [
        animate('0.5s')
      ]),
      transition('* => open', [
        animate('1s',
          style({opacity: '*'}),
        ),
      ]),
      transition('* => *', [
        animate('1s')
      ]),
    ]), trigger('flyInOut', [
      state('in', style({
        transform: 'translateX(0)'
      })),
      transition('void => *', [
        style({transform: 'translateX(-100%)'}),
        animate(100)
      ]),
      transition('* => void', [
        animate(100, style({transform: 'translateX(100%)'}))
      ])
    ]),
    trigger('myInsertRemoveTrigger', [
      transition(':enter', [
        style({opacity: 0}),
        animate('1s', style({opacity: 1})),
      ]),
      transition(':leave', [
        animate('1s', style({opacity: 0}))
      ])
    ]),
    trigger('keyframesDemo', [
      transition('* => active', [
        animate('2s', keyframes([
          style({ backgroundColor: 'blue', offset: 0}),
          style({ backgroundColor: 'red', offset: 0.8}),
          style({ backgroundColor: 'orange', offset: 1.0})
        ])),
      ]),
      transition('* => inactive', [
        animate('2s', keyframes([
          style({ backgroundColor: 'orange', offset: 0}),
          style({ backgroundColor: 'red', offset: 0.2}),
          style({ backgroundColor: 'blue', offset: 1.0})
        ]))
      ]),
    ])
  ]
})
export class TransitionTriggerComponent implements OnInit {
  isShow = true;
  isOpen = true;
  isActive = true;
  boxes = [{}];
  isDisabled = false;
  @HostBinding('@.disabled')
  public animationsDisabled = false;

  constructor() {
  }

  ngOnInit() {
  }

  onAnimationStart(event: AnimationEvent) {
    console.log('animation event start.');
  }

  onAnimationEnd(event: AnimationEvent) {
    console.log('animation event end.');
  }

}
