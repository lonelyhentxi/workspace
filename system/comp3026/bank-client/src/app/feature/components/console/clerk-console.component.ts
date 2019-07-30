import {AfterViewInit, Component, ElementRef, OnInit, ViewChild} from '@angular/core';
import {toCanvas} from 'qrcode';
import {Actor, ChainbankAgentService, Privilege, requestProgress} from '@app/feature/services/chainbank-agent';
import {Router} from '@angular/router';
import {NzNotificationService} from 'ng-zorro-antd';

@Component({
  selector: 'app-clerk-console',
  templateUrl: './clerk-console.component.html',
  styleUrls: ['./clerk-console.component.scss', './console.components.scss']
})
export class ClerkConsoleComponent implements AfterViewInit, OnInit {

  constructor(
    private readonly chainbank: ChainbankAgentService,
    private readonly router: Router,
    private readonly notification: NzNotificationService,
  ) {
  }

  listOfData: Actor[] = [];
  createClerkVisible = false;
  createCustomerVisible = false;
  customerAddressToCreate = '';
  clerkAddressToCreate = '';

  @ViewChild('coverCanvas', {static: false})
  coverCanvas: ElementRef;

  ngOnInit(): void {
    this.refreshTable();
  }

  async ngAfterViewInit() {
    await toCanvas(this.coverCanvas.nativeElement,
      this.chainbank.actor.identity, {
        width: 256
      });
  }

  refreshTable() {
    this.listOfData = this.chainbank.checkActors();
  }

  toggleCreateCustomer() {
    this.createCustomerVisible = !this.createCustomerVisible;
  }

  async confirmCreateCustomer(identity: string) {
    await this.enableActor({identity,privilege: Privilege.Customer});
    this.toggleCreateCustomer();
  }

  toggleCreateClerk() {
    this.createClerkVisible = !this.createClerkVisible;
  }

  async confirmCreateClerk(identity: string) {
    await this.enableActor({identity,privilege: Privilege.Clerk});
    this.toggleCreateClerk();
  }

  async enableActor(actor: { identity, privilege }) {
    const taskFunc = () => this.chainbank.enableActor(actor.identity, actor.privilege);
    await requestProgress(taskFunc, this.notification);
    this.refreshTable();
  }

  async disableActor(actor: { identity, privilege }) {
    const taskFunc = () => this.chainbank.disableActor(actor.identity, actor.privilege);
    await requestProgress(taskFunc, this.notification);
    this.refreshTable();
  }

  canEdit(privilege: Privilege) {
    if(privilege===Privilege.Customer) {
      return this.chainbank.canEditCustomer();
    } else if(privilege===Privilege.Clerk) {
      return this.chainbank.canEditClerk();
    } else {
      return false;
    }
  }

  checkActor(actor: Actor) {
    this.router.navigate(['..', 'customer', actor.identity]);
  }
}
