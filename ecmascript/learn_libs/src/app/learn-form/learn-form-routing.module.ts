import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';
import {FormOverviewComponent} from './form-overview/form-overview.component';

const routes: Routes = [{
  path: '',
  component: FormOverviewComponent,
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnFormRoutingModule {
}
