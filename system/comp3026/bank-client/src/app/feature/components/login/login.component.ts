import {Component, OnInit} from '@angular/core';
import {LocalStorageService} from '../../services/local-storage.service';
import {NzMessageService, NzNotificationService} from 'ng-zorro-antd';
import {ChainbankAgentService} from '@app/feature/services/chainbank-agent/chainbank-agent.service';
import {Privilege} from '@app/feature/services/chainbank-agent/chainbank.interfaces';
import {Router} from '@angular/router';
import {loginProgress} from '@app/feature/services/chainbank-agent';
import {HttpClient, HttpHeaders} from '@angular/common/http';


@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit {

  detailedSetting = false;
  privateKey = '';
  api = '';
  contract = '';
  loadingPrompt = '';

  constructor(
    private readonly localStorage: LocalStorageService,
    private readonly messageService: NzMessageService,
    private readonly chainbank: ChainbankAgentService,
    private readonly notificationService: NzNotificationService,
    private readonly router: Router,
    private readonly httpClient: HttpClient,
  ) {
  }

  async ngOnInit() {
    this.api = this.localStorage.getItemOrDefault('api_address', this.chainbank.defaultApi);
    let contract;
    try {
      contract = this.localStorage.getItemOrDefault('contract_address', await this.getDefaultContract(this.api));
    } catch (e) {
      console.log(e);
    }
    this.contract = contract ? contract : '';
  }

  async resetSetting() {
    this.api = this.chainbank.defaultApi;
    try {
      this.contract = await this.getDefaultContract(this.api);
    } catch(e) {
      console.log(e);
    }
    this.saveSetting();
  }

  async getDefaultContract(api: string): Promise<string> {
    try {
      if (!this.chainbank.validateApiAddress(api)) {
        throw new Error('invalid api address format');
      }
      const contractLink = 'https://' + (this.api.endsWith('/') ? api + 'default_contract' : api + '/default_contract');
      const contract = await this.httpClient.get(contractLink, {
        responseType: 'text'
      }).toPromise() as string;
      if (!this.chainbank.validateContractFormat(contract)) {
        throw new Error('invalid contact format.');
      } else {
        return contract;
      }
    } catch (e) {
      this.notificationService.create('error', 'Load Default Contract Fail', e.message);
      throw e;
    }
  }

  saveSetting() {
    this.localStorage.setItem('api_address', this.api);
    this.localStorage.setItem('contract_address', this.contract);
  }

  toggleSetting() {
    this.detailedSetting = !this.detailedSetting;
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


  alertInvalidFormat(typename: string): void {
    this.notificationService.create('error', 'Failed', `invalid ${typename} format.`);
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

  isLoading() {
    return this.loadingPrompt !== '' && this.loadingPrompt;
  }

  async login() {
    this.loadingPrompt = 'checking format of settings...';
    if (!this.chainbank.validateKeyFormat(this.privateKey)) {
      this.alertInvalidKey();
    } else if (!this.chainbank.validateContractFormat(this.contract)) {
      this.alertInvalidContract();
    } else if (!this.chainbank.validateApiAddress(this.api)) {
      this.alertInvalidApi();
    } else {
      try {
        for await (const prompt of loginProgress(this.chainbank, this.notificationService, this.privateKey, this.api, this.contract)) {
          this.loadingPrompt = prompt;
        }
        this.loadingPrompt = 'successfully logged in, jumping...';
        this.router.navigate(['/', 'console', this.chainbank.actor.privilege === Privilege.Customer ? 'customer' : 'clerk']);
      } catch (e) {
        this.loadingPrompt = '';
        throw  e;
      }
    }
    this.loadingPrompt = '';
  }

  async setDefaultContract() {
    this.contract = await this.getDefaultContract(this.api);
  }
}
