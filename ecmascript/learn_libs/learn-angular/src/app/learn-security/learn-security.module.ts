import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnSecurityRoutingModule } from './learn-security-routing.module';
import { SecurityOverviewComponent } from './security-overview/security-overview.component';

@NgModule({
  declarations: [SecurityOverviewComponent],
  imports: [
    CommonModule,
    LearnSecurityRoutingModule,
  ]
})
export class LearnSecurityModule { }
