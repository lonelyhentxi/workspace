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
        redirectTo: 'learn-web-worker',
        pathMatch: 'full'
      },
      {
        path: 'learn-web-worker',
        loadChildren: () => import('./learn-web-worker/learn-web-worker.module')
          .then(m=>m.LearnWebWorkerModule)
      },
      {
        path: 'learn-service-workers',
        loadChildren: () => import('./learn-service-workers/learn-service-workers.module')
          .then(m=>m.LearnServiceWorkersModule)
      },
      {
        path: 'learn-i18n',
        loadChildren: () => import('./learn-i18n/learn-i18n.module')
          .then(m=>m.LearnI18nModule)
      },
      {
        path: 'learn-security',
        loadChildren: () => import('./learn-security/learn-security.module')
          .then(m=>m.LearnSecurityModule)
      },
      {
        path: 'learn-animation',
        loadChildren: () => import('./learn-animation/learn-animation.module')
          .then(m=>m.LearnAnimationModule)
      },
      {
        path: 'learn-form',
        loadChildren: () => import('./learn-form/learn-form.module')
          .then(m=>m.LearnFormModule)
      },
      {
        path: 'learn-pipes',
        loadChildren: () => import('./learn-pipes/learn-pipes.module')
          .then(m => m.LearnPipesModule)
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
