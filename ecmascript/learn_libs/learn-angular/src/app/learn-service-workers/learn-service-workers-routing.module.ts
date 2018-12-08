import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {ServiceWorkersDemoComponent} from './service-workers-demo/service-workers-demo.component';

const routes: Routes = [{
  path: '',
  component: ServiceWorkersDemoComponent,
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnServiceWorkersRoutingModule { }
