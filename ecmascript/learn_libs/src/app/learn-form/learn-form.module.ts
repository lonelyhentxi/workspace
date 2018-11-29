import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnFormRoutingModule } from './learn-form-routing.module';
import { FormOverviewComponent } from './form-overview/form-overview.component';
import {FormsModule, ReactiveFormsModule} from '@angular/forms';

@NgModule({
  declarations: [FormOverviewComponent],
  imports: [
    CommonModule,
    LearnFormRoutingModule,
    ReactiveFormsModule,
    FormsModule
  ]
})
export class LearnFormModule { }
