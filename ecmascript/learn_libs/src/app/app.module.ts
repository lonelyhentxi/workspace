import { BrowserModule } from '@angular/platform-browser';
import {LOCALE_ID, NgModule} from '@angular/core';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import {BrowserAnimationsModule} from '@angular/platform-browser/animations';
import {registerLocaleData} from '@angular/common';
import localeZhHans from '@angular/common/locales/zh-Hans';
import localeZhHansExtra from '@angular/common/locales/extra/zh-Hans';
registerLocaleData(localeZhHans, 'zh-Hans', localeZhHansExtra);

@NgModule({
  declarations: [
    AppComponent,
  ],
  providers: [
    {
      provide: LOCALE_ID, useValue: 'zh-Hans'
    }
  ],
  imports: [
    BrowserModule,
    BrowserAnimationsModule,
    AppRoutingModule
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
