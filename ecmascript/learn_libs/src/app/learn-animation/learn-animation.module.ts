import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';

import {LearnAnimationRoutingModule} from './learn-animation-routing.module';
import {AnimationDemoComponent} from './animation-demo/animation-demo.component';
import { TransitionTriggerComponent } from './transition-trigger/transition-trigger.component';
import { ComplexSequencesComponent } from './complex-sequences/complex-sequences.component';

@NgModule({
  declarations: [AnimationDemoComponent, TransitionTriggerComponent, ComplexSequencesComponent],
  imports: [
    CommonModule,
    LearnAnimationRoutingModule
  ]
})
export class LearnAnimationModule {
}
