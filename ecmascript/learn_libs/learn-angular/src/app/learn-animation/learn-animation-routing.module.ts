import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {AnimationDemoComponent} from './animation-demo/animation-demo.component';
import {TransitionTriggerComponent} from './transition-trigger/transition-trigger.component';
import {ComplexSequencesComponent} from './complex-sequences/complex-sequences.component';

const routes: Routes = [
  {
    path: '',
    component: ComplexSequencesComponent,
  },
  {
    path: 'transition-trigger',
    component: TransitionTriggerComponent
  },
  {
  path: 'overview',
  component: AnimationDemoComponent
  }
  ];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnAnimationRoutingModule { }
