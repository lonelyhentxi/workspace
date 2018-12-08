import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';
import {SecurityOverviewComponent} from './security-overview/security-overview.component';

const routes: Routes = [{
  path: '',
  component: SecurityOverviewComponent,
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnSecurityRoutingModule {
}
