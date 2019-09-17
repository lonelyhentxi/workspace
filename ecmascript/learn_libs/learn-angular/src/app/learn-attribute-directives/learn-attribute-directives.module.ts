import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnAttributeDirectivesRoutingModule } from './learn-attribute-directives-routing.module';
import { HighlightDirective } from './highlight/highlight.directive';
import { HighlightDemoComponent } from './highlight-demo/highlight-demo.component';

@NgModule({
  declarations: [HighlightDirective, HighlightDemoComponent],
  imports: [
    CommonModule,
    LearnAttributeDirectivesRoutingModule
  ]
})
export class LearnAttributeDirectivesModule { }
