import {Injectable} from '@angular/core';
import {Contract, Wavelet} from 'wavelet-client';
import JSBI from 'jsbi';
import {LocalStorageService} from '@app/feature/services/local-storage.service';
import {AppConfig} from '@app/../environments/environment';
import {ActorNotExistsException, PermissionDeniedException, TimeoutException} from './chainbank.exceptions';
import {Actor, Privilege, Transaction} from './chainbank.interfaces';
import {Subject} from 'rxjs';

@Injectable()
export class ChainbankAgentService {

  static GAS_LIMIT = 250000;
  static formatFactor = 2;
  static keyLength = 64;
  static addressLength = 32;
  static hexCharSet = '[\\da-fA-F]';
  readonly defaultApi = 'blockchain.evernightfireworks.com';
  readonly defaultContract = '140dd223f902ac573d24f7c754ba85ba3f36de90914d6884d149f3dfa772ba80';
  readonly defaultTimeout = 10000;

  wallet;
  contract: Contract;
  wavelet: Wavelet;
  actor: Actor = null;
  updateSubscriber = new Subject();

  constructor(
    private readonly localStorage: LocalStorageService,
  ) {
    if (!AppConfig.production) {
      this.actor = localStorage.getItem<Actor>('dev_actor');
    }
    this.updateSubscriber.subscribe(next => {
      console.log(next);
    });
  }

  validateKeyFormat(key: string): boolean {
    return this.validateU8Array(key, ChainbankAgentService.keyLength);
  }

  validateContractFormat(contractAddress: string): boolean {
    return this.validateU8Array(contractAddress, ChainbankAgentService.addressLength);
  }

  validateAddressFormat(address: string): boolean {
    return this.validateU8Array(address, ChainbankAgentService.addressLength);
  }

  canEditClerk(): boolean {
    return this.actor.privilege === Privilege.Admin;
  }

  canEditCustomer(): boolean {
    return this.actor.privilege === Privilege.Admin || this.actor.privilege === Privilege.Clerk;
  }

  private validateU8Array(content: string, len: number): boolean {
    const reg = new RegExp(`^${ChainbankAgentService.hexCharSet}{${ChainbankAgentService.formatFactor * len}}$`);
    return Boolean(content.match(reg));
  }

  validateApiAddress(apiAddress: string): boolean {
    const uri = `https://${apiAddress}`;
    try {
      const _ = new URL(uri);
    } catch (e) {
      return false;
    }
    return true;
  }

  async* login(privateKey: string, apiAddress: string, contractAddress: string) {
    yield 'linking to chain nodes...';
    const client = new Wavelet('https://' + apiAddress);
    yield 'loading wallet...';
    const wallet = Wavelet.loadWalletFromPrivateKey(privateKey);
    yield 'loading contract...';
    const contract = new Contract(client, contractAddress);
    await contract.init();
    yield 'init your actor...';
    this.wavelet = client;
    this.wallet = wallet;
    this.contract = contract;
    this.checkActor();
    if (!AppConfig.production) {
      this.localStorage.setItem('dev_actor', this.actor);
    }
  }

  checkActor() {
    try {
      const results = this.exposeResult(this.contract.test(this.wallet, 'get_actor', JSBI.BigInt(0)));
      this.actor = Actor.parseLog(results[0]);
    } catch (e) {
      throw new ActorNotExistsException();
    }
  }

  exposeResult(res): string[] {
    return res['logs'] as any;
  }

  async* enableActor(identity: string, privilege: Privilege) {
    this.validateAddressFormat(identity);
    let funcName;
    if (privilege === Privilege.Clerk) {
      funcName = 'enable_clerk';
    } else if (privilege === Privilege.Customer) {
      funcName = 'enable_customer';
    } else {
      throw new PermissionDeniedException();
    }
    yield await this.contract.call(
      this.wallet,
      funcName,
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(ChainbankAgentService.GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'raw', value: identity}
    );
    return await this.detectRoundEnd();
  }

  async* disableActor(identity: string, privilege: Privilege) {
    this.validateAddressFormat(identity);
    let funcName;
    if (privilege === Privilege.Clerk) {
      funcName = 'disable_clerk';
    } else if (privilege === Privilege.Customer) {
      funcName = 'disable_customer';
    } else {
      throw new PermissionDeniedException();
    }
    yield await this.contract.call(
      this.wallet,
      funcName,
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(ChainbankAgentService.GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'raw', value: identity},
    );
    return await this.detectRoundEnd();
  }

  detectRoundEnd() {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        reject(new TimeoutException());
      }, this.defaultTimeout);
      this.wavelet.pollConsensus({
        onRoundEnded: message => {
          this.contract.fetchAndPopulateMemoryPages().then(c => {
            resolve(message);
          }).catch(e => {
            reject(e);
          });
        }
      });
    });
  }

  async transfer(sender: string, receiver: string, amount: JSBI) {
    this.validateAddressFormat(sender);
    this.validateAddressFormat(receiver);
    await this.contract.call(
      this.wallet,
      'transfer',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(ChainbankAgentService.GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'raw', value: sender},
      {type: 'raw', value: receiver},
      {type: 'raw', value: JSBI.toString()}
    );
  }

  checkActors(): Actor[] {
    try {
      const results = this.exposeResult(
        this.contract.test(this.wallet, 'get_actors', JSBI.BigInt(0), []));
      const actorsStr = results[0] ? results[0].split('\n') : [];
      return actorsStr.map(a => Actor.parseLog(a));
    } catch (e) {
      throw new PermissionDeniedException();
    }
  }

  checkTransactions(identity: string): Transaction[] {
    this.validateAddressFormat(identity);
    try {
      const results = this.exposeResult(
        this.contract.test(this.wallet, 'get_transactions', JSBI.BigInt(0), [{type: 'raw', value: identity}]));
      const transactionsStr = results[0] ? results[0].split('\n') : [];
      return transactionsStr.map(tl => Transaction.parseLog(tl));
    } catch (e) {
      throw new PermissionDeniedException();
    }
  }

  * checkBalance(identity: string): IterableIterator<Transaction[] | JSBI> {
    this.validateAddressFormat(identity);
    let transactions;
    try {
      transactions = yield this.checkTransactions(identity);
    } catch (e) {
      throw new PermissionDeniedException();
    }
    let balance = JSBI.BigInt(0);
    for (const t of transactions) {
      if (t.sender === identity) {
        balance = JSBI.subtract(balance, JSBI.BigInt(t.amount));
      } else if (t.receiver === identity) {
        balance = JSBI.add(balance, JSBI.BigInt(t.amount));
      } else {
        throw new TypeError('transaction must relate to identity');
      }
    }
    return balance;
  }
}
