import {Injectable} from '@angular/core';
import {AbstractControl, AsyncValidator, ValidationErrors} from '@angular/forms';
import {Observable, timer} from 'rxjs';
import {map, take} from 'rxjs/operators';

@Injectable()
export class UniqueAlterEgoValidator implements AsyncValidator {
  constructor(/* inject services */) {
  }

  validate(ctrl: AbstractControl): Observable<ValidationErrors | null> {
    if (ctrl.value === 'superman') {
      return timer(1000).pipe(take(1), map(() => ({uniqueAlterEgo: true})));
    } else {
      return timer(1000).pipe(take(1), map(() => null));
    }
  }
}
