import {Component, OnInit} from '@angular/core';
import {Actor, ChainbankAgentService, mutationProgress, Privilege, syncProgress} from '@app/feature/services/chainbank-agent';
import {ActivatedRoute, Router} from '@angular/router';
import {NzNotificationService} from 'ng-zorro-antd';

@Component({
  selector: 'app-clerk-console',
  templateUrl: './clerk-console.component.html',
  styleUrls: ['./clerk-console.component.scss']
})
export class ClerkConsoleComponent implements OnInit {

  constructor(
    private readonly chainbank: ChainbankAgentService,
    private readonly router: Router,
    private readonly notification: NzNotificationService,
    private readonly route: ActivatedRoute,
  ) {
  }

  listOfData: Actor[] = [];
  createClerkVisible = false;
  createCustomerVisible = false;
  customerAddressToCreate = '';
  clerkAddressToCreate = '';

  async ngOnInit() {
    await this.refreshTable(true);
  }

  async refreshTable(syncing = false) {
    if (syncing) {
      await syncProgress(this.chainbank, this.notification);
    }
    this.listOfData = this.chainbank.checkActors();
  }

  toggleCreateCustomer() {
    this.createCustomerVisible = !this.createCustomerVisible;
  }

  async confirmCreateCustomer(identity: string) {
    await this.enableActor({identity, privilege: Privilege.Customer});
    this.toggleCreateCustomer();
  }

  toggleCreateClerk() {
    this.createClerkVisible = !this.createClerkVisible;
  }

  async confirmCreateClerk(identity: string) {
    await this.enableActor({identity, privilege: Privilege.Clerk});
    this.toggleCreateClerk();
  }

  async enableActor(actor: { identity, privilege }) {
    const taskFunc = () => this.chainbank.enableActor(actor.identity, actor.privilege);
    await mutationProgress(taskFunc, this.notification);
    await this.refreshTable(false);
  }

  async disableActor(actor: { identity, privilege }) {
    const taskFunc = () => this.chainbank.disableActor(actor.identity, actor.privilege);
    await mutationProgress(taskFunc, this.notification);
    await this.refreshTable(false);
  }

  canEdit(privilege: Privilege) {
    if (privilege === Privilege.Customer) {
      return this.chainbank.canEditCustomer();
    } else if (privilege === Privilege.Clerk) {
      return this.chainbank.canEditClerk();
    } else {
      return false;
    }
  }

  checkActor(actor: Actor) {
    console.log(actor);
    this.router.navigate(['customer', actor.identity], {relativeTo: this.route.parent});
  }
}
