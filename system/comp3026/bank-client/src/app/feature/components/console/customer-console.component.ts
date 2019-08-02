import {Component, OnInit} from '@angular/core';
import {ActivatedRoute, Router} from '@angular/router';
import {ChainbankAgentService, mutationProgress, syncProgress, Transaction} from '@app/feature/services/chainbank-agent';
import {NzNotificationService} from 'ng-zorro-antd';
import JSBI from 'jsbi';

@Component({
  selector: 'app-customer-console',
  templateUrl: './customer-console.component.html',
  styleUrls: ['./customer-console.component.scss']
})
export class CustomerConsoleComponent implements OnInit {

  constructor(
    private readonly route: ActivatedRoute,
    private readonly chainbank: ChainbankAgentService,
    private readonly notification: NzNotificationService,
    private readonly router: Router,
  ) {
  }

  identity: string;
  transactions: Transaction[] = [];
  balance: JSBI = JSBI.BigInt(0);
  receiver = '';
  amount = 0;
  maxAmount = 0;
  minAmount = this.chainbank.amountStep;

  async ngOnInit() {
    const id = this.route.snapshot.paramMap.get('id');
    this.identity = id ? id : this.chainbank.actor.identity;
    await this.checkBalance(id);
  }


  async checkBalance(identity: string, syncing = true): Promise<void> {
    if(syncing) {
      await syncProgress(this.chainbank, this.notification);
    }
    const task = this.chainbank.checkBalance(identity);
    this.transactions = task.next().value as Transaction[];
    const balance = task.next().value as JSBI;
    this.balance = balance;
    this.maxAmount = this.numericJSBI(balance);
  }


  numericJSBI(balance: JSBI): number {
    return JSBI.toNumber(balance);
  }

  validateAmount(amount: number): boolean {
    return amount >= this.minAmount && amount <= this.maxAmount;
  }

  backToClerkConsole() {
    this.router.navigate(['clerk'], {relativeTo: this.route.parent});
  }

  async commitTransaction() {
    this.validateAmount(this.amount);
    this.chainbank.validateAddressFormat(this.receiver);
    const taskFunc = () => this.chainbank.transfer(this.identity, this.receiver, JSBI.BigInt(this.amount));
    await mutationProgress(taskFunc, this.notification);
    await this.checkBalance(this.identity, false);
  }
}

