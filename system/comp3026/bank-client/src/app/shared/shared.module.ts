import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {TranslateModule} from '@ngx-translate/core';
import {PageNotFoundComponent} from './components/';
import {WebviewDirective} from './directives/';
import {ExitButtonComponent} from './components/exit-button/exit-button.component';
import {FrameLogoComponent } from './components/frame-logo/frame-logo.component';
import {NgZorroAntdModule} from 'ng-zorro-antd';
import {FormsModule} from '@angular/forms';

@NgModule({
  declarations: [
    PageNotFoundComponent,
    WebviewDirective,
    ExitButtonComponent,
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
    ExitButtonComponent,
    FrameLogoComponent,
    NgZorroAntdModule,
  ]
})
export class SharedModule {
}
