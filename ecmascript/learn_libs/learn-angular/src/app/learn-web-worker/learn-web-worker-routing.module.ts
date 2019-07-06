import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {WebWorkerDemoComponent} from './web-worker-demo.component';

const routes: Routes = [{
  path: '',
  component: WebWorkerDemoComponent
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnWebWorkerRoutingModule { }
