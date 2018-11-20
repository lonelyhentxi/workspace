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
        redirectTo: 'learn-structural-directives',
        pathMatch: 'full'
      },
      {
        path: 'learn-structural-directives',
        loadChildren: () => import('./learn-structural-directives/learn-structural-directives.module')
          .then(m => m.LearnStructuralDirectivesModule)
      },
      {
        path: 'learn-attribute-directives',
        loadChildren: () => import('./learn-attribute-directives/learn-attribute-directives.module')
          .then(m => m.LearnAttributeDirectivesModule)
      },
      {
        path: 'learn-element',
        loadChildren: () => import('./learn-element/learn-element.module').then(m => m.LearnElementModule)
      },
      {
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
