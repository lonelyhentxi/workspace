import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';
import {AppComponent} from './app.component';

const routes: Routes = [
  {
    path: '',
    component: AppComponent,
    children: [
      {
        path: '',
        redirectTo: 'learn-element',
        pathMatch: 'full'
      }, {
        path: 'learn-element',
        loadChildren: () => import('./learn-element/learn-element.module').then(m => m.LearnElementModule)
      }, {
        path: 'rxjs',
        loadChildren: () => import('./rxjs/rxjs.module').then(m => m.RxjsModule),
      }
    ]
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
