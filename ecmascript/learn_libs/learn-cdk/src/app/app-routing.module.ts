import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';
import {LayoutComponent} from './layout/layout.component';
import {ObservablesComponent} from './observables/observables.component';
import {ScrollingComponent} from './scrolling/scrolling.component';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    redirectTo: 'scrolling'
  }, {
    path: 'layout',
    component: LayoutComponent
  }, {
    path: 'observables',
    component: ObservablesComponent
  }, {
    path: 'scrolling',
    component: ScrollingComponent
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
