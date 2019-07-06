import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnWebWorkerRoutingModule } from './learn-web-worker-routing.module';
import { WebWorkerDemoComponent } from './web-worker-demo.component';
import {FormsModule} from '@angular/forms';

@NgModule({
  declarations: [WebWorkerDemoComponent],
  imports: [
    CommonModule,
    LearnWebWorkerRoutingModule,
    FormsModule
  ]
})
export class LearnWebWorkerModule { }
