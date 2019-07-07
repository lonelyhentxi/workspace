import {Directive, ViewContainerRef} from '@angular/core';

@Directive({
  selector: '[appAdHost]'
})
export class AdDirective {

  constructor(public viewContainerRef: ViewContainerRef) { }

}
