import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {LearnPipeDemoComponent} from './learn-pipe-demo/learn-pipe-demo.component';

const routes: Routes = [
  {
    path: '',
    component: LearnPipeDemoComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnPipesRoutingModule { }
