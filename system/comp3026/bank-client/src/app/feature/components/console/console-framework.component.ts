import { Component} from '@angular/core';

@Component({
  selector: 'app-console-framework',
  template: `
      <div nz-row class="console-main">
          <div nz-col nzSpan="5" nzOffset="1">
              <app-console-profile></app-console-profile>
          </div>
          <div nz-col nzSpan="16" nzOffset="1">
              <router-outlet></router-outlet>
          </div>
      </div>

  `,
  styles: [`
      .console-main {
          margin-top: 30px;
      }
  `]
})
export class ConsoleFrameworkComponent {
}
