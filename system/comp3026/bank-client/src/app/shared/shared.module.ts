import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {TranslateModule} from '@ngx-translate/core';
import {PageNotFoundComponent} from './components/';
import {WebviewDirective} from './directives/';
import {ControlButtonsComponent} from './components/control-buttons/control-buttons.component';
import {FrameLogoComponent } from './components/frame-logo/frame-logo.component';
import {NgZorroAntdModule} from 'ng-zorro-antd';
import {FormsModule} from '@angular/forms';

@NgModule({
  declarations: [
    PageNotFoundComponent,
    WebviewDirective,
    ControlButtonsComponent,
    FrameLogoComponent
  ],
  imports: [
    FormsModule,
    CommonModule,
    TranslateModule,
    NgZorroAntdModule,
  ],
  exports: [
    FormsModule,
    TranslateModule,
    WebviewDirective,
    ControlButtonsComponent,
    FrameLogoComponent,
    NgZorroAntdModule,
  ]
})
export class SharedModule {
}
