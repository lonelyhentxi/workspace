import {Component, TemplateRef} from '@angular/core';
import {ElectronService} from '@core/services/electron/electron.service';

@Component({
  selector: 'app-exit-button',
  template: `
      <div class="app-exit" (click)="beforeExitApp()">
          <img src="../../../../assets/img/app_exit.png" alt="app-exit">
      </div>
      <nz-modal [(nzVisible)]="showModal" nzTitle="{{('COMMON.EXIT'|translate).toUpperCase()}}"
                [nzKeyboard]="false" [nzMaskClosable]="false"
                nzContent="{{'COMMON.CONFIRM'|translate}} {{'COMMON.EXIT'|translate}}?"
                nzOkText="{{('COMMON.CONFIRM'|translate).toUpperCase()}}"
                nzCancelText="{{('COMMON.NOT'|translate).toUpperCase()}}" (nzOnOk)="exit()"
                (nzOnCancel)="cancelExit()" class="exit-modal"
      >
      </nz-modal>
  `,
  styles: [`
      .app-exit {
        cursor: pointer;
        -webkit-app-region: no-drag;
        position: fixed;
        z-index: 100;
        right: 24px;
        top: 24px;
        width: 20px;
        height: 20px;
      }

      .app-exit img {
          position: fixed;
          height: 20px;
      }

      .app-exit :hover img {
          opacity: 0.5 !important;
          background-color: transparent !important;
          filter: alpha(opacity=50);
      }

      .exit-modal {
          -webkit-app-region: no-drag;
      }
  `]
})
export class ExitButtonComponent {
  showModal = false;

  constructor(
    private readonly electron: ElectronService
  ) { }

  beforeExitApp() {
    this.showModal = true;
  }

  cancelExit() {
    this.showModal = false;
  }

  exit() {
    this.electron.remote.app.exit(0);
  }
}
