import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { LayoutModule } from '@angular/cdk/layout';
import { ObserversModule } from '@angular/cdk/observers';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatButtonModule } from '@angular/material/button';
import { ScrollingModule } from '@angular/cdk/scrolling';

import { LayoutComponent } from './layout/layout.component';
import { ObservablesComponent } from './observables/observables.component';
import { ObservablesContentComponent } from './observables/observables-content.component';
import { ObservablesWrapperComponent } from './observables/observables-wrapper.component';
import { ScrollingComponent } from './scrolling/scrolling.component';

@NgModule({
  declarations: [
    AppComponent,
    LayoutComponent,
    ObservablesComponent,
    ObservablesContentComponent,
    ObservablesWrapperComponent,
    ScrollingComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    LayoutModule,
    ObserversModule,
    BrowserAnimationsModule,
    MatButtonModule,
    ScrollingModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
