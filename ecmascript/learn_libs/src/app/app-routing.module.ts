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
        redirectTo: 'learn-dcl',
        pathMatch: 'full'
      }, {
        path: 'learn-element',
        loadChildren: () => import('./learn-element/learn-element.module').then(m => m.LearnElementModule)
      }, {
        path: 'rxjs',
        loadChildren: () => import('./rxjs/rxjs.module').then(m => m.RxjsModule),
      },
      {
        path: 'learn-dcl',
        loadChildren: () => import('./learn-dynamic-component-loader/learn-dynamic-component-loader.module')
          .then(m => m.LearnDynamicComponentLoaderModule)
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
