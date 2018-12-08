import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { LearnFormRoutingModule } from './learn-form-routing.module';
import { FormOverviewComponent } from './form-overview/form-overview.component';
import {FormsModule, NG_VALIDATORS, ReactiveFormsModule} from '@angular/forms';
import { FormValidatorComponent } from './form-validator/form-validator.component';
import { ForbiddenValidatorDirective } from './form-validator/forbidden-validator.directive';
import { IdentityRevealedDirective } from './form-validator/identity-revealed.directive';
import {UniqueAlterEgoValidator} from './form-validator/unique-alter-ego.validator';

@NgModule({
  declarations: [
    FormOverviewComponent,
    FormValidatorComponent,
    ForbiddenValidatorDirective,
    IdentityRevealedDirective,
  ],
  providers: [
    UniqueAlterEgoValidator,
  ],
  imports: [
    CommonModule,
    LearnFormRoutingModule,
    ReactiveFormsModule,
    FormsModule
  ]
})
export class LearnFormModule { }
