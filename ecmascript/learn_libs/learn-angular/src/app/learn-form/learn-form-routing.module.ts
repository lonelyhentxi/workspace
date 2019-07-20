import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';
import {FormOverviewComponent} from './form-overview/form-overview.component';
import {FormValidatorComponent} from './form-validator/form-validator.component';

const routes: Routes = [{
  path: 'overview',
  component: FormOverviewComponent,
},{
  path: '',
  component: FormValidatorComponent,
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnFormRoutingModule {
}
