import {Component, HostBinding, OnInit} from '@angular/core';
import {animate, group, query, stagger, state, style, transition, trigger} from '@angular/animations';

@Component({
  selector: 'app-complex-sequences',
  template: `
    <div>
      <ul>
        <li class="hero" *ngFor="let hero of _heroes">{{hero.name}}</li>
      </ul>
    </div>
    <div>
      <h2 *ngIf="showGroupAnimation" @flyInOut>Group Animation Demo</h2>
      <button (click)="showGroupAnimation=!showGroupAnimation">Toggle Group Animation</button>
    </div>
    <div>
      <ul class="heroes" [@filterAnimation]="heroTotal">
          <li *ngFor="let hero of _heroes">{{hero.name}}</li>
      </ul>
      <button (click)="heroTotal=(heroTotal+1)%7-1">Toggle Filter Animation</button>
    </div>
  `,
  styles: [],
  animations: [
    trigger('pageAnimations', [
      transition(':enter', [
        query('.hero', [
          style({opacity: 0, transform: 'translateX(-100px)'}),
          stagger(-30, [
            animate('500ms cubic-bezier(0.35, 0, 0.25, 1)',
              style({ opacity: 1, transform: 'none' }))
          ])
        ])
      ])
    ]),
    trigger('flyInOut', [
      state('in', style({
        width: 120,
        transform: 'translateX(0)', opacity: 1
      })),
      transition('void => *', [
        style({ width: 10, transform: 'translateX(50px)', opacity: 0 }),
        group([
          animate('0.3s 0.1s ease', style({
            transform: 'translateX(0)',
            width: 120
          })),
          animate('0.3s ease', style({
            opacity: 1
          }))
        ])
      ]),
      transition('* => void', [
        group([
          animate('0.3s ease', style({
            transform: 'translateX(50px)',
            width: 10
          })),
          animate('0.3s 0.2s ease', style({
            opacity: 0
          }))
        ])
      ])
    ]),
    trigger('filterAnimation', [
      transition(':enter, * => 0, * => -1', []),
      transition(':increment', [
        query(':enter', [
          style({ opacity: 0, width: '0px' }),
          stagger(50, [
            animate('300ms ease-out', style({ opacity: 1, width: '*' })),
          ]),
        ], { optional: true })
      ]),
      transition(':decrement', [
        query(':leave', [
          stagger(50, [
            animate('300ms ease-out', style({ opacity: 0, width: '0px' })),
          ]),
        ])
      ]),
    ]),
  ]
})
export class ComplexSequencesComponent implements OnInit {
  @HostBinding('@pageAnimations')
  public animatePage = true;
  showGroupAnimation = true;
  showFilterAnimation = true;

  _heroes = [];
  heroTotal = -1;
  get heroes() {
    return this._heroes;
  }

  ngOnInit() {
    this._heroes = Array(10).fill({})
      .map(_=>({name: Math.floor(Math.random()*100000).toString()}));
  }

  updateCriteria(criteria: string) {
    criteria = criteria ? criteria.trim() : '';

    this._heroes = this._heroes.filter(hero => hero.name.toLowerCase().includes(criteria.toLowerCase()));
    const newTotal = this.heroes.length;

    if (this.heroTotal !== newTotal) {
      this.heroTotal = newTotal;
    } else if (!criteria) {
      this.heroTotal = -1;
    }
  }

}
