import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { LearnI18nRoutingModule } from './learn-i18n-routing.module';
import { I18nOverviewComponent } from './i18n-overview/i18n-overview.component';

@NgModule({
  declarations: [I18nOverviewComponent],
  imports: [
    CommonModule,
    LearnI18nRoutingModule,
  ]
})
export class LearnI18nModule { }
