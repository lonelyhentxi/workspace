import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { FeatureRoutingModule } from './feature-routing.module';
import { LoginComponent } from './components/login.component';
import { SharedModule } from '../shared/shared.module';
import {ChainbankAgentService} from './services/chainbank-agent.service';
import {LocalStorageService} from './services/local-storage.service';


@NgModule({
  declarations: [
    LoginComponent,
  ],
  imports: [
    CommonModule,
    SharedModule,
    FeatureRoutingModule,
  ],
  providers: [
    ChainbankAgentService,
    LocalStorageService,
  ]
})
export class FeatureModule { }
