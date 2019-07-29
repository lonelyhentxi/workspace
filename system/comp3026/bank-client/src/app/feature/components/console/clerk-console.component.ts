import {AfterViewInit, Component, ElementRef, OnInit, ViewChild} from '@angular/core';
import {toCanvas} from 'qrcode';
import {Actor, ChainbankAgentService, Privilege} from '@app/feature/services/chainbank-agent';

@Component({
  selector: 'app-clerk-console',
  templateUrl: './clerk-console.component.html',
  styleUrls: ['./clerk-console.component.scss', './console.components.scss']
})
export class ClerkConsoleComponent implements AfterViewInit, OnInit {

  listOfData: Actor[] = [];

  ngOnInit(): void {
    Array(100).fill(0).forEach(()=>this.addRow());
  }

  @ViewChild('coverCanvas', {static: false})
  coverCanvas: ElementRef;

  constructor(
    private readonly chainbank: ChainbankAgentService,
  ) {
  }

  async ngAfterViewInit() {
    await toCanvas(this.coverCanvas.nativeElement,
      this.chainbank.actor.identity, {
        width: 256
      });
  }

  addRow(): void {
    this.listOfData = [
      ...this.listOfData,
      {
        identity: this.chainbank.actor.identity,
        privilege: this.chainbank.actor.privilege,
        enabled: this.chainbank.actor.enabled,
      }
    ];
  }

  enableCustomer() {

  }

  enableClerk() {

  }

  disableActor(id: string): void {
    this.listOfData = this.listOfData.filter(d => d.identity !== id);
  }

  canDisable(privilege: Privilege) {
    const selfPrivilege = this.chainbank.actor.privilege;
    if(selfPrivilege===Privilege.Clerk) {
      return privilege===Privilege.Customer;
    } else if(selfPrivilege===Privilege.Admin) {
      return privilege!==Privilege.Admin;
    } else {
      return false;
    }
  }
}
