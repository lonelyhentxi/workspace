import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnPipesRoutingModule } from './learn-pipes-routing.module';
import { LearnPipeDemoComponent } from './learn-pipe-demo/learn-pipe-demo.component';
import { ExponentialStrengthPipe } from './exponential-strength/exponential-strength.pipe';
import { FlyingHeroesImpurePipe } from './flying-heroes-impure/flying-heroes-impure.pipe';

@NgModule({
  declarations: [LearnPipeDemoComponent, ExponentialStrengthPipe, FlyingHeroesImpurePipe],
  imports: [
    CommonModule,
    LearnPipesRoutingModule
  ]
})
export class LearnPipesModule { }
