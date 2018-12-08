import { Directive } from '@angular/core';
import {AbstractControl, NG_VALIDATORS, ValidationErrors} from '@angular/forms';
import {identityRevealedValidator} from './identity-revealed.validator';

@Directive({
  selector: '[appIdentityRevealed]',
  providers: [
    {
      provide: NG_VALIDATORS,
      useExisting: IdentityRevealedDirective,
      multi: true,
    }
  ]
})
export class IdentityRevealedDirective {
  validate(control: AbstractControl): ValidationErrors {
    return identityRevealedValidator(control);
  }
}
