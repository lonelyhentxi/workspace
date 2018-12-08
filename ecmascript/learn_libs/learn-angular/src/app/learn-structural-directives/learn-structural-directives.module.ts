import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnStructuralDirectivesRoutingModule } from './learn-structural-directives-routing.module';
import { StructuralDirectivesDemoComponent } from './structural-directives-demo/structural-directives-demo.component';
import {FormsModule} from '@angular/forms';
import { UnlessDirective } from './structural-directives-demo/unless.directive';

@NgModule({
  declarations: [StructuralDirectivesDemoComponent, UnlessDirective],
  imports: [
    CommonModule,
    LearnStructuralDirectivesRoutingModule,
    FormsModule
  ]
})
export class LearnStructuralDirectivesModule { }
