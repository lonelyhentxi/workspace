import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';

const routes: Routes = [{
  path: '',
  redirectTo: 'learn-element',
  pathMatch: 'full'
}, {
  path: 'rxjs',
  loadChildren: () => import('./rxjs/rxjs.module').then(m => m.RxjsModule),
},
  {
    path: 'learn-element',
    loadChildren: () => import('./learn-element/learn-element.module').then(m => m.LearnElementModule)
  }];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
