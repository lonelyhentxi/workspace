import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { RxjsCh1Component } from './rxjs-ch1/rxjs-ch1.component';
import { RxjsCh4Component } from './rxjs-ch4/rxjs-ch4.component';
import { RxjsCh5Component } from './rxjs-ch5/rxjs-ch5.component';
import { RxjsCh8Component } from './rxjs-ch8/rxjs-ch8.component';

const routes: Routes = [{
  path: 'rxjs',
  redirectTo:'rxjs/ch15',
  pathMatch:'full'
},{
  path: 'rxjs/ch1',
  component:RxjsCh1Component 
},{
  path: 'rxjs/ch4',
  component:RxjsCh4Component
},{
  path: 'rxjs/ch5',
  component: RxjsCh5Component
},{
    path: 'rxjs/ch8',
    component: RxjsCh8Component
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class RxjsRoutingModule { }
