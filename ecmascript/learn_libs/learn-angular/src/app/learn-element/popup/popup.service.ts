import {ApplicationRef, ComponentFactoryResolver, Injectable, Injector} from '@angular/core';
import {PopupComponent} from './popup.component';
import {NgElement, WithProperties} from '@angular/elements';

@Injectable()
export class PopupService {
  constructor(
    private injector: Injector,
    private applicationRef: ApplicationRef,
    private componentFactoryResolver: ComponentFactoryResolver) {
  }

  showAsComponent(message: string) {
    const popup = document.createElement('popup-component');
    const factory = this.componentFactoryResolver.resolveComponentFactory(PopupComponent);
    const popupComponentRef =factory.create(this.injector, [],  popup);
    this.applicationRef.attachView(popupComponentRef.hostView);
    popupComponentRef.instance.closed.subscribe(()=>{
      document.body.removeChild(popup);
      this.applicationRef.detachView(popupComponentRef.hostView);
    });
    popupComponentRef.instance.message = message;
    document.body.appendChild(popup);
  }

  showAsElement(message: string) {
    const popupEl: NgElement & WithProperties<PopupComponent> = document.createElement('popup-element') as any;
    popupEl.addEventListener('closed', ()=>document.body.removeChild(popupEl));
    popupEl.message = message;
    document.body.appendChild(popupEl);
  }
}
