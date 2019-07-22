import { Injectable } from '@angular/core';
import { Wavelet, Contract } from 'wavelet-client';
import {cli} from 'webdriver-manager/built/lib/cli_instance';

@Injectable()
export class ChainbankAgentService {

  private static hexCharSet = new Set('1234567890abcdefABCDEF');
  readonly defaultApi = 'blockchain.evernightfireworks.com';
  readonly defaultContract = '433d4d8425a290952e52c0575b7ac689847d07ad8cd059ff35ba700660515b8d';

  wallet;
  contract: Contract;
  wavelet: Wavelet;
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
    const client = new Wavelet('https://'+apiAddress);
    const wallet = Wavelet.loadWalletFromPrivateKey(privateKey);
    const contract = new Contract(client, contractAddress);
    await contract.init();
    this.wavelet = client;
    this.wallet = wallet;
    this.contract = contract;
  }
}
