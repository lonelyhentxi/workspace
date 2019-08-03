import {Component} from '@angular/core';
import {Router} from '@angular/router';
import { shell } from 'electron';

@Component({
  selector: 'app-frame-logo',
  template: `
      <div class="frame-logo">
          <div (click)="toSite()">
              <img src="../../../../assets/img/chainbank_logo.svg" width="40" height="40"/>
          </div>
          <div (click)="toSite()">
              <img src="../../../../assets/img/chainbank_title.svg" width="156" height="48"/>
          </div>
      </div>
  `,
  styles: [`
      .frame-logo {
          position: absolute;
          top: 8px;
          left: 16px;
          width: 104px;
          height: 48px;
      }

      .frame-logo > div:nth-child(1) {
          display: block;
          position: absolute;
          top: 12px;
          left: 16px;
          height: 40px;
          width: 40px;
          font-weight: bolder;
          -webkit-app-region: no-drag;
          cursor: pointer;
          z-index: 100;
      }

      .frame-logo > div:nth-child(2) {
          display: block;
          position: absolute;
          top: 8px;
          left: 56px;
          height: 48px;
          -webkit-app-region: no-drag;
          cursor: pointer;
          z-index: 100;
      }
  `]
})
export class FrameLogoComponent {

  constructor(
    private readonly router: Router,
  ) {
  }

  async toSite() {
    await shell.openExternal('https//github.com/lonelyhentai/chainbank');
  }
}
