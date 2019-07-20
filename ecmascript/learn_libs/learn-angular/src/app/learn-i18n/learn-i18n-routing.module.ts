import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {I18nOverviewComponent} from './i18n-overview/i18n-overview.component';

const routes: Routes = [{
  path: '',
  component: I18nOverviewComponent,
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnI18nRoutingModule { }
