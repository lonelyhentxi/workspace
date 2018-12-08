import {NgElement, WithProperties} from '@angular/elements';

declare global {
  interface HTMLElmentTagNameMap {
    'popup-element': NgElement & WithProperties<{message: string}>;
  }
}
