import {Component} from '@angular/core';
import {Flyer} from '../flying-heroes-impure/heroes';
import {interval, Observable} from 'rxjs';
import {map, take} from 'rxjs/operators';

@Component({
  selector: 'app-learn-pipe-demo',
  template: `
    <p>The hero's default is {{ birthday | date: format | uppercase }}</p>
    <div>
      <button (click)="toggleFormat()">Toggle Format</button>
    </div>
    <h2>Power Booster</h2>
    <p>Super power boost: {{2 | exponentialStrength: 10}}</p>
    <div *ngFor="let hero of (pipeHeroes | flyingHeroesImpure)">
      {{hero.name}}
    </div>
    <h2>Async Hero Message and AsyncPipe</h2>
    <p>Message: {{ message$ | async }}</p>
    <div><button (click)="resend()">Resend</button></div>
  `,
  styles: []
})
export class LearnPipeDemoComponent {
  message$: Observable<string>;

  private messages = [
    'You are my hero!',
    'You are the best hero!',
    'Will you be my hero?',
  ];

  pipeHeroes: Flyer[] = [
    {
      name: '王民从',
      emotion: 'funny',
      canFly: true,
    },
    {
      name: '刘嘉熠',
      emotion: 'sad',
      canFly: false,
    },
    {
      name: '崔天宇',
      emotion: 'common',
      canFly: true,
    }
  ];

  constructor() {
  }

  birthday = new Date();
  toggle = true;

  get format() {
    return this.toggle ? 'shortDate' : 'fullDate';
  }

  toggleFormat() {
    this.toggle = !this.toggle;
  }

  resend() {
    this.message$ = interval(500).pipe(
      map(i => this.messages[i]),
      take(this.messages.length)
    );
  }
}
