import {Injectable} from '@angular/core';
import {Contract, Wavelet} from 'wavelet-client';
import JSBI from 'jsbi';
import {LocalStorageService} from '@app/feature/services/local-storage.service';
import {AppConfig} from '@app/../environments/environment';
import {ActorNotExistsException, PermissionDeniedException, TimeoutException} from './chainbank.exceptions';
import {Actor, Transaction} from './chainbank.interfaces';
import {Subject} from 'rxjs';

const GAS_LIMIT = 100;

@Injectable()
export class ChainbankAgentService {

  private static hexCharSet = new Set('1234567890abcdefABCDEF');
  readonly defaultApi = 'blockchain.evernightfireworks.com';
  readonly defaultContract = '5c96230b6fffeaeb7434cce26e1b52dd07cbc82257ff9a4854840432bea81373';

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
    return this.validateU8Array(key, 64);
  }

  validateContractFormat(contractAddress: string): boolean {
    return this.validateU8Array(contractAddress, 32);
  }

  validateAddressFormat(address: string): boolean {
    return this.validateU8Array(address, 32);
  }

  private validateU8Array(content: string, len: number): boolean {
    if (content.length !== len * 2) {
      return false;
    } else {
      return [...content].every(c => ChainbankAgentService.hexCharSet.has(c));
    }
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
    this.wavelet.pollConsensus({
      onRoundEnded: message => {
        this.contract.fetchAndPopulateMemoryPages().then(_ => {
          this.updateSubscriber.next(message);
        });
      }
    });
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

  async enableClerk(identity: string) {
    this.validateAddressFormat(identity);
    await this.contract.call(
      this.wallet,
      'enable_clerk',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'bytes', value: identity}
    );
  }

  async disableClerk(identity: string) {
    this.validateAddressFormat(identity);
    await this.contract.call(
      this.wallet,
      'disable_clerk',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'bytes', value: identity}
    );
  }

  async enableCustomer(identity: string) {
    this.validateAddressFormat(identity);
    await this.contract.call(
      this.wallet,
      'enable_customer',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'bytes', value: identity}
    );
  }

  async disableCustomer(identity: string) {
    this.validateAddressFormat(identity);
    await this.contract.call(
      this.wallet,
      'disable_customer',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'bytes', value: identity},
    );
  }

  async transfer(sender: string, receiver: string, amount: JSBI) {
    this.validateAddressFormat(sender);
    this.validateAddressFormat(receiver);
    await this.contract.call(
      this.wallet,
      'transfer',
      JSBI.BigInt(0), // amount to send
      JSBI.BigInt(GAS_LIMIT), // gas limit
      JSBI.BigInt(0), // gas deposit (not explained)
      {type: 'bytes', value: sender},
      {type: 'bytes', value: receiver},
      {type: 'bytes', value: JSBI.toString()}
    );
  }

  checkTransactions(identity: string): Transaction[] {
    this.validateAddressFormat(identity);
    try {
      const results = this.exposeResult(
        this.contract.test(this.wallet, 'get_transactions', JSBI.BigInt(0), [{type: 'bytes', value: identity}]));
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
