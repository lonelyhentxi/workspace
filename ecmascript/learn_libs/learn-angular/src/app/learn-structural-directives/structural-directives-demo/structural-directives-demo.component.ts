import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'app-structural-directives-demo',
  template: `
    <p>
      I turned the corner
      <ng-container *ngIf="hero">
        and saw {{hero.name}}. I waved
      </ng-container>
      and continued on my way.
    </p>
    <div>
      Pick your favorite hero
      (<label><input type="checkbox" checked (change)="showSad=!showSad">show sad</label>)
    </div>
    <div>
      <select [(ngModel)]="hero">
        <ng-container *ngFor="let h of heros">
          <ng-container *ngIf="showSad || h.emotion != 'sad'">
            <option [ngValue]="h">{{h.name}} ({{h.emotion}})</option>
          </ng-container>
        </ng-container>
      </select>
    </div>
    <button (click)="condition=!condition">{{condition?'condition true':'condition false'}}</button>
    <p *appUnless="condition" class="unless a">
      (A) This paragraph is displayed because the condition is false.
    </p>
    <p *appUnless="!condition" class="unless b">
      (B) Although the condition is true,
      this paragraph is displayed because appUnless is set to false.
    </p>
  `,
})
export class StructuralDirectivesDemoComponent implements OnInit {

  hero;
  heros: { name: string, emotion: string }[] = [
    {name: '动感超人', emotion: 'funny'},
    {name: '奥特曼', emotion: 'sad'}
  ];
  showSad = true;
  condition: boolean = true;

  constructor() {
  }

  ngOnInit() {
  }

}
