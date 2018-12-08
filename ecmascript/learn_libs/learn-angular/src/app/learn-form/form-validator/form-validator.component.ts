import {Component, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from '@angular/forms';
import {forbiddenNameValidator} from './forbidden-name.validator';
import {identityRevealedValidator} from './identity-revealed.validator';
import {UniqueAlterEgoValidator} from './unique-alter-ego.validator';

@Component({
  selector: 'app-form-validator',
  template: `
    <div class="container">
      <h1>Custom Validator Demo With Reactive</h1>
      <form [formGroup]="heroForm">
        <div class="form-group">
          <label>
            Name
            <input type="text" class="form-control" formControlName="name">
          </label>
          <div
            [hidden]="heroForm.get('name').valid || heroForm.get('name').pristine" class="alert alert-danger"
          >
            Name is {{heroForm.get('name').errors|json }}
          </div>
        </div>
        <div class="form-group">
          <label>
            alterEgo
            <input type="text" class="form-control" formControlName="alterEgo">
          </label>
        </div>
        <div *ngIf="heroForm.errors?.identityRevealed && (heroForm.touched || 
        heroForm.dirty)" class="cross-validation-error-message alert alert-danger">
          Name cannot match alter ego
        </div>
      </form>
    </div>
    <div class="container">
      <h1>Custom Validator Demo With Template</h1>
      <form #heroTemplateForm="ngForm" appIdentityRevealed>
        <div class="form-group">
          <label>
            Name
            <input type="text" class="form-control" id="name"
                   [(ngModel)]="model.name" #name1="ngModel"
                   name="name" required maxlength="4" appForbiddenName="bob">
          </label>
          <div
            [hidden]="name1.valid || name1.pristine" class="alert alert-danger"
          >
            Name is {{name1.errors|json }}
          </div>
        </div>
        <div *ngIf="heroTemplateForm.errors?.identityRevealed 
        && (heroTemplateForm.touched ||
        heroTemplateForm.dirty)" class="cross-validation-error-message alert alert-danger">
          Name cannot match alert ego.
        </div>
      </form>
    </div>
    <div class="container">
      <h1>Async Validator</h1>
      <form [formGroup]="asyncForm">
        <div class="form-group">
          <input type="text" formControlName="name" class="form-control">
          <div *ngIf="asyncForm.get('name').errors?.uniqueAlterEgo && (asyncForm.get('name').touched || asyncForm.get('name').dirty)"
               class="alert alert-danger">
            async name
          </div>
          <div *ngIf="asyncForm.get('name').pending" class="spinner-grow" style="width: 3rem; height: 3rem;" role="status">
            <span class="sr-only">Loading...</span>
          </div>
        </div>
      </form>
    </div>
  `,
  styles: [
      `
      @import url('https://unpkg.com/bootstrap@4.2.1/dist/css/bootstrap.min.css');
    `
  ]
})
export class FormValidatorComponent {
  heroForm = new FormGroup({
    'name': new FormControl('lab', [
      Validators.required,
      Validators.maxLength(20),
      forbiddenNameValidator(/bob/i)
    ]),
    'alterEgo': new FormControl(''),
    'power': new FormControl('')
  }, {
    validators: identityRevealedValidator
  });

  model = {
    name: 'lab',
    alterEgo: ''
  };

  asyncForm = new FormGroup({
    name: new FormControl('lab', {
      asyncValidators: [this.uniqueAlterEgoValidator.validate.bind(this.uniqueAlterEgoValidator)],
      updateOn: 'blur'
    }),
  });

  constructor(
    private readonly uniqueAlterEgoValidator: UniqueAlterEgoValidator,
  ) {
  }
}
