import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnDynamicComponentLoaderRoutingModule } from './learn-dynamic-component-loader-routing.module';
import { AdDirective } from './ad/ad.directive';
import { AdComponent } from './ad/ad.component';
import { AdItemComponent } from './ad-item/ad-item.component';

@NgModule({
  declarations: [AdDirective, AdComponent, AdItemComponent],
  imports: [
    CommonModule,
    LearnDynamicComponentLoaderRoutingModule
  ],
  entryComponents: [AdItemComponent]
})
export class LearnDynamicComponentLoaderModule { }
