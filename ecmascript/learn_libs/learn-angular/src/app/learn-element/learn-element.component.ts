import {Component, Injector} from '@angular/core';
import {PopupService} from './popup/popup.service';
import {createCustomElement} from '@angular/elements';
import {PopupComponent} from './popup/popup.component';

@Component({
  selector: 'app-learn-element',
  template: `
    <input #input value="Message">
    <button (click)="popup.showAsComponent(input.value)">Show as component</button>
    <button (click)="popup.showAsElement(input.value)">Show as element</button>
  `
})
export class LearnElementComponent {

  constructor(injector: Injector, public popup: PopupService) {
    const PopupElement = createCustomElement(PopupComponent, {injector});
    customElements.define('popup-element', PopupElement);
  }
}
