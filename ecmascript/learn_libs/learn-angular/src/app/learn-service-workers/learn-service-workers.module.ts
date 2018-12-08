import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnServiceWorkersRoutingModule } from './learn-service-workers-routing.module';
import { ServiceWorkersDemoComponent } from './service-workers-demo/service-workers-demo.component';

@NgModule({
  declarations: [ServiceWorkersDemoComponent],
  imports: [
    CommonModule,
    LearnServiceWorkersRoutingModule
  ]
})
export class LearnServiceWorkersModule { }
