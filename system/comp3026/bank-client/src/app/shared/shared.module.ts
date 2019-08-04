import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {WebviewDirective} from './directives/';
@NgModule({
  declarations: [
    WebviewDirective,
  ],
  imports: [
    CommonModule,
  ],
  exports: [
    WebviewDirective,
  ]
})
export class SharedModule {
}
