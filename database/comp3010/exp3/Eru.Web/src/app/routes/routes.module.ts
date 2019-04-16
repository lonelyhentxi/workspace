import { NgModule } from '@angular/core';

import { SharedModule } from '@shared';
import { RouteRoutingModule } from './routes-routing.module';
// dashboard pages
import { DashboardComponent } from './dashboard/dashboard.component';
// passport pages
import { UserLoginComponent } from './passport/login/login.component';
import { UserRegisterComponent } from './passport/register/register.component';
// single pages
import { CallbackComponent } from './callback/callback.component';
import { PostComponent } from './post/post.component';
import { NzCommentActionHostDirective, NzCommentModule } from 'ng-zorro-antd';

const COMPONENTS = [
  DashboardComponent,
  // passport pages
  UserLoginComponent,
  UserRegisterComponent,
  // single pages
  CallbackComponent,
];
const COMPONENTS_NOROUNT = [];

@NgModule({
  imports: [ SharedModule, RouteRoutingModule,NzCommentModule],
  declarations: [
    ...COMPONENTS,
    ...COMPONENTS_NOROUNT,
    PostComponent
  ],
  entryComponents: COMPONENTS_NOROUNT
})
export class RoutesModule {}
