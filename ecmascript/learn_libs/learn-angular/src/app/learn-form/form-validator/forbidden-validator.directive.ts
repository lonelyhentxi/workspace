import {Directive, Input} from '@angular/core';
import {AbstractControl, NG_VALIDATORS} from '@angular/forms';
import {forbiddenNameValidator} from './forbidden-name.validator';

@Directive({
  selector: '[appForbiddenName]',
  providers: [{provide: NG_VALIDATORS, useExisting: ForbiddenValidatorDirective, multi: true}],
})
export class ForbiddenValidatorDirective {

  @Input('appForbiddenName') forbiddenName: string;

  validate(control: AbstractControl): { [key: string]: any } | null {
    return this.forbiddenName ? forbiddenNameValidator(new RegExp(this.forbiddenName, 'i'))(control) : null;
  }
}
