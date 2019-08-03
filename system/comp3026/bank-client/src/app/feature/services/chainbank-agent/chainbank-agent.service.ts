import {Injectable} from '@angular/core';
import {Contract, Wallet, Wavelet} from 'wavelet-client';
import JSBI from 'jsbi';
import {LocalStorageService} from '@app/feature/services/local-storage.service';
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
  readonly defaultContract = 'd7f7badd0b5f1010ef8ac21cd2920398ae0edfe8508726a7d59307d54558425e';
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
    this.wavelet = client;
    this.wallet = wallet;
    this.contract = contract;
    yield 'checking your public key...';
    try {
      const results = this.exposeResult(this.contract.test(this.wallet, 'get_actor', JSBI.BigInt(0)));
      this.actor = Actor.parseLog(results[0]);
    } catch (e) {
      throw new ActorNotExistsException();
    }
    yield 'validating your private key...';
    await this.contract.call(
      this.wallet,
      'get_actor',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(this.GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
    );
    yield 'syncing round...';
    await this.detectRoundEnd();
    return 'logged in';
  }

  exposeResult(res): string[] {
    if (res['result'] || res['result'] === '') {
      throw new Error(res['result']);
    }
    return res['logs'] as string[];
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
    let funcName;
    if (privilege === Privilege.Clerk) {
      funcName = 'enable_clerk';
    } else if (privilege === Privilege.Customer) {
      funcName = 'enable_customer';
    } else {
      throw new PermissionDeniedException();
    }
    yield await this.testCall(
      funcName,
      [{type: 'raw', value: identity}]
    );
    return await this.detectRoundEnd();
  }

  async* disableActor(identity: string, privilege: Privilege): AsyncIterableIterator<ContractCallResult | RoundEndMessage> {
    let funcName;
    if (privilege === Privilege.Clerk) {
      funcName = 'disable_clerk';
    } else if (privilege === Privilege.Customer) {
      funcName = 'disable_customer';
    } else {
      throw new PermissionDeniedException();
    }
    yield await this.testCall(
      funcName,
      [{type: 'raw', value: identity}]
    );
    return await this.detectRoundEnd();
  }

  async* transfer(sender: string, receiver: string, amount: JSBI): AsyncIterableIterator<ContractCallResult | RoundEndMessage> {
    yield await this.testCall(
      'transfer',
      [
        {type: 'raw', value: sender},
        {type: 'raw', value: receiver},
        {type: 'string', value: amount.toString()}
      ]
    );
    return await this.detectRoundEnd();
  }

  checkActors(): Actor[] {
    try {
      const results = this.exposeResult(
        this.contract.test(this.wallet, 'get_actors', JSBI.BigInt(0)));
      const actorsStr = results[0] ? results[0].split('\n') : [];
      return actorsStr.map(a => Actor.parseLog(a));
    } catch (e) {
      throw new PermissionDeniedException();
    }
  }

  * checkBalance(identity: string): IterableIterator<Transaction[] | JSBI> {
    let transactions;
    const results = this.exposeResult(
      this.contract.test(this.wallet, 'get_transactions', JSBI.BigInt(0), {type: 'raw', value: identity}));
    const logs = results[0].split('\n').filter(x => x !== '');
    let balance = JSBI.BigInt(logs[0]);
    logs.splice(0, 1);
    transactions = logs.map(l=>Transaction.parseLog(l));
    yield transactions;
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

  async testCall(fnName: string, params: { type: string, value: any }[]): Promise<ContractCallResult> {
    let testRes;
    try {
      testRes = this.contract.test(this.wallet, fnName, JSBI.BigInt(0), ...params);
    } catch {
      throw new Error('contract params parse error');
    }
    if (testRes['result']) {
      throw new Error(testRes['result']);
    }
    return await this.contract.call(
      this.wallet, fnName,
      JSBI.BigInt(0), JSBI.BigInt(this.GAS_LIMIT), JSBI.BigInt(0),
      ...params
    );
  }
}
