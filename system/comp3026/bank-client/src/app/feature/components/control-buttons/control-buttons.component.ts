import {Component, TemplateRef} from '@angular/core';
import {ElectronService} from '@core/services/electron/electron.service';
import {Router} from '@angular/router';

@Component({
  selector: 'app-control-buttons',
  template: `
      <div class="app-logout control-button" (click)="logout()">
          <img src="assets/img/app_logout.png" alt="app-logout">
      </div>
      <div class="app-exit control-button" (click)="beforeExitApp()">
          <img src="assets/img/app_exit.png" alt="app-exit">
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
        .control-button {
            -webkit-app-region: no-drag;
            cursor: pointer;
            position: fixed;
            z-index: 100;
        }
    
      .app-exit {
        right: 24px;
        top: 24px;
        width: 20px;
        height: 20px;
      }
      
      .app-logout {
          right: 56px;
          top: 24px;
          width: 20px;
          height: 20px;
      }

      .control-button img {
          position: fixed;
          height: 20px;
      }
      
      .control-button img {
          opacity: 0.5 !important;
          background-color: transparent !important;
          filter: alpha(opacity=50);
      }
      
      .exit-modal {
          -webkit-app-region: no-drag;
      }
  `]
})
export class ControlButtonsComponent {
  showModal = false;

  constructor(
    private readonly electron: ElectronService,
    private readonly router: Router,
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

  logout() {
    this.router.navigate(['/']);
  }
}
