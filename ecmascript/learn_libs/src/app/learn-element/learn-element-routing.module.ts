import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {LearnElementComponent} from './learn-element.component';

const routes: Routes = [{
  path: '',
  component: LearnElementComponent,
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnElementRoutingModule { }
