import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';

import {LearnElementRoutingModule} from './learn-element-routing.module';
import {PopupComponent} from './popup/popup.component';
import {PopupService} from './popup/popup.service';
import {LearnElementComponent} from './learn-element.component';

@NgModule({
  declarations: [PopupComponent, LearnElementComponent],
  providers: [PopupService],
  imports: [
    CommonModule,
    LearnElementRoutingModule
  ],
  entryComponents: [
    PopupComponent
  ]
})
export class LearnElementModule {
}
