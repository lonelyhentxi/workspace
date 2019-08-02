import {Injectable} from '@angular/core';
import {Contract, Wavelet, Wallet} from 'wavelet-client';
import JSBI from 'jsbi';
import {LocalStorageService} from '@app/feature/services/local-storage.service';
import {AppConfig} from '@app/../environments/environment';
import {ActorNotExistsException, PermissionDeniedException, TimeoutException} from './chainbank.exceptions';
import {Actor, ContractCallResult, Privilege, RoundEndMessage, Transaction} from './chainbank.interfaces';

@Injectable()
export class ChainbankAgentService {

  readonly GAS_LIMIT = 250000;
  readonly formatFactor = 2;
  readonly keyLength = 64;
  readonly addressLength = 32;
  readonly hexCharSet = '[\\da-fA-F]';
  readonly defaultApi = 'blockchain.evernightfireworks.com';
  readonly defaultContract = '140dd223f902ac573d24f7c754ba85ba3f36de90914d6884d149f3dfa772ba80';
  readonly defaultTimeout = 10000;
  readonly amountStep = 0.01;


  wallet: Wallet;
  contract: Contract;
  wavelet: Wavelet;
  actor: Actor = null;


  constructor(
    private readonly localStorage: LocalStorageService,
  ) {
  }


  validateKeyFormat(key: string): boolean {
    return this.validateU8Array(key, this.keyLength);
  }

  validateContractFormat(contractAddress: string): boolean {
    return this.validateU8Array(contractAddress, this.addressLength);
  }

  validateAddressFormat(address: string): boolean {
    return this.validateU8Array(address, this.addressLength);
  }

  canEditClerk(): boolean {
    return this.actor.privilege === Privilege.Admin;
  }

  canEditCustomer(): boolean {
    return this.actor.privilege === Privilege.Admin || this.actor.privilege === Privilege.Clerk;
  }

  private validateU8Array(content: string, len: number): boolean {
    if (typeof content !== 'string') {
      return false;
    }
    const reg = new RegExp(`^${this.hexCharSet}{${this.formatFactor * len}}$`);
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

  async* login(privateKey: string, apiAddress: string, contractAddress: string): AsyncIterableIterator<string> {
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
      this.localStorage.setItem('dev_settings',[privateKey, apiAddress, contractAddress]);
    }
  }

  checkActor(): void {
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

  detectRoundEnd(): Promise<RoundEndMessage> {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        reject(new TimeoutException());
      }, this.defaultTimeout);
      this.wavelet.pollConsensus({
        onRoundEnded: message => {
          this.contract.fetchAndPopulateMemoryPages().then(_ => {
            resolve(message);
          }).catch(e => {
            reject(e);
          });
        }
      });
    });
  }

  async syncContract(): Promise<void> {
    await this.contract.fetchAndPopulateMemoryPages();
  }

  async* enableActor(identity: string, privilege: Privilege): AsyncIterableIterator<ContractCallResult | RoundEndMessage> {
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
      JSBI.BigInt(this.GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'raw', value: identity}
    );
    return await this.detectRoundEnd();
  }

  async* disableActor(identity: string, privilege: Privilege): AsyncIterableIterator<ContractCallResult | RoundEndMessage> {
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
      JSBI.BigInt(this.GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'raw', value: identity},
    );
    return await this.detectRoundEnd();
  }

  async *transfer(sender: string, receiver: string, amount: JSBI):AsyncIterableIterator<ContractCallResult| RoundEndMessage> {
    this.validateAddressFormat(sender);
    this.validateAddressFormat(receiver);
    yield await this.contract.call(
      this.wallet,
      'transfer',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(this.GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'raw', value: sender},
      {type: 'raw', value: receiver},
      {type: 'raw', value: JSBI.toString()}
    );
    return await this.detectRoundEnd();
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
    } catch
      (e) {
      throw new PermissionDeniedException();
    }
  }

  * checkBalance(identity: string): IterableIterator<Transaction[] | JSBI> {
    this.validateAddressFormat(identity);
    let transactions;
    try {
      transactions = this.checkTransactions(identity);
      yield transactions;
    } catch (e) {
      throw new PermissionDeniedException();
    }
    let balance = JSBI.BigInt(0);
    for (const t of transactions) {
      if (t.sender === identity) {
        balance = JSBI.subtract(balance, t.amount);
      } else if (t.receiver === identity) {
        balance = JSBI.add(balance, t.amount);
      } else {
        throw new TypeError('transaction must relate to identity');
      }
    }
    return balance;
  }

  async devLoadMock(): Promise<void> {
    if (!AppConfig.production) {
      const devSettings = this.localStorage.getItem<[string, string, string]>('dev_settings');
      if (devSettings) {
        for await(const _ of this.login(...devSettings)) {}
      }
    }
  }
}
