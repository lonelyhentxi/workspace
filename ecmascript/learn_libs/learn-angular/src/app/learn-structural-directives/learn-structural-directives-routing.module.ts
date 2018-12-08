import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {StructuralDirectivesDemoComponent} from './structural-directives-demo/structural-directives-demo.component';

const routes: Routes = [
  {
    path: '',
    component: StructuralDirectivesDemoComponent,
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnStructuralDirectivesRoutingModule { }
