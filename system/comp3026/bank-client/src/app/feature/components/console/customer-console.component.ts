import { Component, OnInit } from '@angular/core';
import {ActivatedRoute} from '@angular/router';
import {ChainbankAgentService} from '@app/feature/services/chainbank-agent';

@Component({
  selector: 'app-customer-console',
  templateUrl: './customer-console.component.html',
  styleUrls: ['./customer-console.component.scss']
})
export class CustomerConsoleComponent implements OnInit {

  constructor(
    private readonly route: ActivatedRoute,
    private readonly chainbank: ChainbankAgentService,
  ) { }

  identity: string;

  ngOnInit() {
    const id = this.route.snapshot.paramMap.get('id');
    this.identity = id?id:this.chainbank.actor.identity;
    console.log(this.identity);
  }
}
