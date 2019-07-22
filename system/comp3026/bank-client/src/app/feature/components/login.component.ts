import {Component, OnInit} from '@angular/core';
import {LocalStorageService} from '../services/local-storage.service';
import {NzMessageService} from 'ng-zorro-antd';
import {ChainbankAgentService} from '../services/chainbank-agent.service';

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
                          <nz-upload [nzBeforeUpload]="beforeImport">
                              <button nz-button>Import File</button>
                          </nz-upload>
                      </div>
                      <div *ngIf="detailedSetting">
                          <h4>Contract Address</h4>
                      </div>
                      <div *ngIf="detailedSetting">
                          <textarea class="transparent-bg"
                                    nz-input
                                    placeholder="Please enter your contract address"
                                    [nzAutosize]="{ minRows: 2, maxRows: 2 }"
                                    [(ngModel)]="contract">
                          </textarea>
                      </div>
                      <div>
                          <h4>Node Address</h4>
                      </div>
                      <div>
                          <input class="transparent-bg"
                                 nz-input [readOnly]="!detailedSetting"
                                 placeholder="Please enter your API address"
                                 [(ngModel)]="api"/>
                      </div>
                      <div class="login-panel-control">
                          <button nz-button nzType="primary" (click)="login()">Login</button>
                          <button nz-button (click)="detailedSetting=!detailedSetting">
                              {{detailedSetting ? 'Hide' : 'Detailed Setting'}}
                          </button>
                          <button nz-button *ngIf="detailedSetting" (click)="saveSetting()">
                              Save Setting
                          </button>
                          <button nz-button *ngIf="detailedSetting" (click)="resetSetting()">
                              Reset
                          </button>
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

      .login-panel-control > button {
          margin-right: 5px;
      }
  `]
})
export class LoginComponent implements OnInit {

  detailedSetting = false;
  privateKey = '87a6813c3b4cf534b6ae82db9b1409fa7dbd5c13dba5858970b56084c4a930eb400056ee68a7cc2695222df05ea76875bc27ec6e61e8e62317c336157019c405';
  api = '';
  contract = '';

  constructor(
    private readonly localStorage: LocalStorageService,
    private readonly messageService: NzMessageService,
    private readonly chainbank: ChainbankAgentService,
  ) {
    this.api = this.localStorage.getItemOrDefault('api_address', this.chainbank.defaultApi);
    this.contract = this.localStorage.getItemOrDefault('contract_address', this.chainbank.defaultContract);
  }

  ngOnInit() {
  }

  resetSetting() {
    this.api = this.chainbank.defaultApi;
    this.contract = this.chainbank.defaultContract;
    this.saveSetting();
  }

  saveSetting() {
    this.localStorage.setItem('api_address', this.api);
    this.localStorage.setItem('contract_address', this.contract);
  }

  beforeImport = (file: File): boolean => {
    const me = this;
    if (file.size > 102400) {
      this.alertInvalidKey();
      return false;
    }
    const reader = new FileReader();
    const loadListener = (e) => {
      const content = e.target.result;
      if (!me.chainbank.validateKeyFormat(content)) {
        me.alertInvalidKey();
      } else {
        me.privateKey = content;
      }
      reader.removeEventListener('loadend', loadListener);
      reader.removeEventListener('error', errorListener);
    };
    reader.addEventListener('loadend', loadListener);
    const errorListener = (e) => {
      me.alertInvalidKey();
      reader.removeEventListener('loadend', loadListener);
      reader.removeEventListener('error', errorListener);
    };
    reader.addEventListener('error', errorListener);
    reader.readAsText(file, 'utf-8');
    return false;
  };

  alertInvalidFormat(typename: string):void {
    this.messageService.warning(`Invalid ${typename} format.`);
  }
  alertInvalidContract(): void {
    this.alertInvalidFormat('contract address');
  }
  alertInvalidKey(): void {
    this.alertInvalidFormat('primary key');
  }

  alertInvalidApi(): void {
    this.alertInvalidFormat('api');
  }

  async login() {
    if(!this.chainbank.validateKeyFormat(this.privateKey)) {
      this.alertInvalidKey();
    } else if(!this.chainbank.validateContractFormat(this.contract)) {
      this.alertInvalidContract();
    } else if(!this.chainbank.validateApiAddress(this.api)) {
      this.alertInvalidApi();
    } else {
      try {
        await this.chainbank.login(this.privateKey,this.api, this.contract);
      } catch (e) {
        console.log(e);
      }
    }
  }
}
