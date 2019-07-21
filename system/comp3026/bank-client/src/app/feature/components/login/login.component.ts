import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'app-login',
  template: `
      <div nz-row>
          <div nz-col nzSpan="10" nzOffset="2">
              <div class="login-welcome">
                  <h1>Welcome to Chainbank</h1>
                  <h3>Please enter your wallet's private key,
                      and ensure your Chainbank node HTTPS API address to continue</h3>
              </div>
          </div>
          <div nz-col nzSpan="10" nzOffset="1">
              <nz-card class="transparent-bg" nzTitle="Login">
                  <div class="login-panel">
                      <div>
                          <h4>Private Key</h4>
                      </div>
                      <div>
                      <textarea class="transparent-bg"
                                nz-input
                                placeholder="Please enter your private key"
                                [(ngModel)]="privateKey"
                                [nzAutosize]="{ minRows: 3, maxRows: 3 }"
                      ></textarea>
                      </div>
                      <div>
                          <button nz-button>Import file</button>
                      </div>
                      <div>
                          <h4>Node Address</h4>
                      </div>
                      <div>
                      <input class="transparent-bg"
                                nz-input
                                placeholder="Please enter your private key"
                                [(ngModel)]="apiAddress"/>
                      </div>
                      <div>
                          <button nz-button nzType="primary">Login</button>
                      </div>
                  </div>
              </nz-card>
          </div>
      </div>

  `,
  styles: [`
      .login-welcome {
          margin-top: 128px;
      }
    .login-panel > div {
        margin-top: 10px;
    }
  `]
})
export class LoginComponent implements OnInit {

  privateKey = '';
  apiAddress = 'blockchain.evernightfireworks.com';

  constructor() {
  }

  ngOnInit() {
  }

}
