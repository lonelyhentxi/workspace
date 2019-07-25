import { Injectable } from '@angular/core';
import { Wavelet, Contract } from 'wavelet-client';
import JSBI from 'jsbi';

export class ActorNotExistsException {
  name: string;
 constructor(readonly message='actor not exists') {
   this.name = 'ActorNotExists';
 }
}

export enum Privilege {
  Admin = 0,
  Clerk = 1,
  Customer = 2
}

export class Actor {

  constructor(
    readonly identity: string,
    readonly privilege: Privilege,
    readonly enabled: boolean) {
  }


  static parseLogs(logs: string[]): Actor {
    const fields = logs[0].split(',');
    const identity = fields[0];
    const privilege = Actor.parsePrivilege(fields[1]);
    const enabled = Actor.parseBool(fields[2]);
    return new Actor(identity, privilege, enabled);
  }

  static parsePrivilege(privilegeStr: string): Privilege {
    const lowerStr = privilegeStr.toLowerCase();
    if(lowerStr==='admin') {
      return Privilege.Admin;
    } else if(lowerStr==='clerk') {
      return Privilege.Clerk;
    } else if(lowerStr==='customer') {
      return Privilege.Customer;
    } else {
      throw new TypeError('error privilege');
    }
  }

  static parseBool(boolStr): boolean {
    const lowerStr = boolStr.toLowerCase();
    if(lowerStr==='true') {
      return true;
    } else if(lowerStr==='false') {
      return false;
    } else {
      throw new TypeError('error boolean');
    }
  }



}

@Injectable()
export class ChainbankAgentService {

  private static hexCharSet = new Set('1234567890abcdefABCDEF');
  readonly defaultApi = 'blockchain.evernightfireworks.com';
  readonly defaultContract = '5c96230b6fffeaeb7434cce26e1b52dd07cbc82257ff9a4854840432bea81373';


  wallet;
  contract: Contract;
  wavelet: Wavelet;
  actor: Actor = null;

  constructor() { }

  validateKeyFormat(key: string): boolean {
    return this.validateU8Array(key, 64);
  }

  validateContractFormat(contractAddress: string): boolean {
    return this.validateU8Array(contractAddress, 32);
  }

  private validateU8Array(content: string, len: number): boolean {
    if(content.length!==len*2) {
      return false;
    } else {
      return [...content].every(c=>ChainbankAgentService.hexCharSet.has(c))
    }
  }

  validateApiAddress(apiAddress:string): boolean {
    const uri = `https://${apiAddress}`;
    try {
      const _ = new URL(uri);
    } catch(e) {
      return false;
    }
    return true;
  }

  async login(privateKey: string, apiAddress: string, contractAddress: string) {
    const client = new Wavelet('https://' + apiAddress);
    const wallet = Wavelet.loadWalletFromPrivateKey(privateKey);
    const contract = new Contract(client, contractAddress);
    await contract.init();
    this.wavelet = client;
    this.wallet = wallet;
    this.contract = contract;
    this.fetchActor();
  }

  fetchActor() {
      try {
        const results = this.exposeResult(this.contract.test(this.wallet, 'get_actor', JSBI.BigInt(0)));
        this.actor = Actor.parseLogs(results);
      } catch (e) {
        throw new ActorNotExistsException();
      }
  }

  exposeResult(res): string[] {
    return res['logs'] as any;
  }
}
