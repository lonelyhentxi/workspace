import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {AdComponent} from './ad/ad.component';

const routes: Routes = [{
  path: '',
  component: AdComponent,
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnDynamicComponentLoaderRoutingModule { }
