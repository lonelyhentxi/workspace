import 'reflect-metadata';
import '../polyfills'

import {BrowserModule} from '@angular/platform-browser';
import {BrowserAnimationsModule} from '@angular/platform-browser/animations';
import {NgModule} from '@angular/core';
import {FormsModule} from '@angular/forms';
import {HttpClient, HttpClientModule} from '@angular/common/http';
import {CoreModule} from '@core/core.module';
import {SharedModule} from '@shared/shared.module';
import {AppRoutingModule} from './app-routing.module';
// NG Translate
import {TranslateLoader, TranslateModule} from '@ngx-translate/core';
import {TranslateHttpLoader} from '@ngx-translate/http-loader';

import {AppComponent} from './app.component';
import {en_US, NgZorroAntdModule, NZ_I18N} from 'ng-zorro-antd';

import {registerLocaleData} from '@angular/common';
import en from '@angular/common/locales/en';
import {LoginComponent} from '@app/feature/components/login/login.component';
import {CustomerConsoleComponent} from '@app/feature/components/console/customer-console.component';
import {ClerkConsoleComponent} from '@app/feature/components/console/clerk-console.component';
import {ChainbankAgentService} from '@app/feature/services/chainbank-agent.service';
import {LocalStorageService} from '@app/feature/services/local-storage.service';

registerLocaleData(en);

// AoT requires an exported function for factories
export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './assets/i18n/', '.json');
}

@NgModule({
  declarations: [
    AppComponent,
    LoginComponent,
    CustomerConsoleComponent,
    ClerkConsoleComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    CoreModule,
    HttpClientModule,
    BrowserAnimationsModule,
    NgZorroAntdModule,
    SharedModule,
    AppRoutingModule,
    TranslateModule.forRoot({
      loader: {
        provide: TranslateLoader,
        useFactory: HttpLoaderFactory,
        deps: [HttpClient]
      }
    })
  ],
  providers: [
    {provide: NZ_I18N, useValue: en_US},
    ChainbankAgentService,
    LocalStorageService,
  ],
  bootstrap: [AppComponent]
})
export class AppModule {
}
