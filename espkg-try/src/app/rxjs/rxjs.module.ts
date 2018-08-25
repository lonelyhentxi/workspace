import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { RxjsRoutingModule } from './rxjs-routing.module';
import { RxjsCh1Component } from './rxjs-ch1/rxjs-ch1.component';
import { RxjsCh4Component } from './rxjs-ch4/rxjs-ch4.component';
import { RxjsCh5Component } from './rxjs-ch5/rxjs-ch5.component';
import { RxjsCh8Component } from './rxjs-ch8/rxjs-ch8.component';

@NgModule({
  imports: [
    CommonModule,
    RxjsRoutingModule
  ],
  declarations: [RxjsCh1Component, RxjsCh4Component, RxjsCh5Component,RxjsCh8Component]
})
export class RxjsModule { }
