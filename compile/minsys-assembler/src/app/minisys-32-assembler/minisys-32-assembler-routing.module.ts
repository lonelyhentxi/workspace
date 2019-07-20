import {NgModule} from '@angular/core';
import {Routes, RouterModule} from '@angular/router';
import {Minisys32AssemblerComponent} from './minisys-32-assembler.component';


const routes: Routes = [
  {
    path: '',
    redirectTo:'minisys',
    pathMatch:'full'
  },{
    path:'minisys',
    component: Minisys32AssemblerComponent,
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class Minisys32AssemblerRoutingModule {
}
