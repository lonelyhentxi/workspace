import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { RxjsCh1Component } from './ch1/rxjs-ch1.component';

const routes: Routes = [{
  path: 'rxjs',
  redirectTo:'rxjs/ch1',
  pathMatch:'full'
},{
  path: 'rxjs/ch1',
  component:RxjsCh1Component 
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class RxjsRoutingModule { }
