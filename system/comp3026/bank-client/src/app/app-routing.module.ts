import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { PageNotFoundComponent } from '@shared/components';
import {LoginComponent} from '@app/feature/components/login/login.component';
import {ClerkConsoleComponent} from '@app/feature/components/console/clerk-console.component';
import {CustomerConsoleComponent} from '@app/feature/components/console/customer-console.component';
import {ConsoleFrameworkComponent} from '@app/feature/components/console/console-framework.component';

const routes: Routes = [
  {
    path: '',
    children: [
      {
        path: '',
        redirectTo: 'login',
        pathMatch: 'full',
      },
      {
        path: 'login',
        component: LoginComponent,
      },
      {
        path: 'console',
        component: ConsoleFrameworkComponent,
        children: [
          {
            path: 'clerk',
            component: ClerkConsoleComponent,
          },
          {
            path: 'customer/:',
            component: CustomerConsoleComponent,
          }
        ]
      },
    ]
  },
  {
    path: '**',
    component: PageNotFoundComponent
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes, { useHash: true, enableTracing: true })],
  exports: [RouterModule]
})
export class AppRoutingModule {}
