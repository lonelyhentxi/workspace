import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';
import {HighlightDemoComponent} from './highlight-demo/highlight-demo.component';

const routes: Routes = [
  {
    path: '',
    redirectTo: 'highlight',
    pathMatch: 'full'
  },
  {
    path: 'highlight',
    component: HighlightDemoComponent,
  }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class LearnAttributeDirectivesRoutingModule {
}
