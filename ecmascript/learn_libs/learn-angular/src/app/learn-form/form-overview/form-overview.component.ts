import {Component, OnInit} from '@angular/core';
import {FormArray, FormBuilder, FormControl, FormGroup, Validators} from '@angular/forms';
import {Observable, Subscription} from 'rxjs';
import {map} from 'rxjs/operators';

@Component({
  selector: 'app-form-overview',
  template: `
    <div>
      Favorite Color With Reactive: <input type="text" [formControl]="favoriteColorControl">
    </div>
    <div>
      Reactive Snapshot With ValueChanges: {{ favoriteColorControlValue$ | async }}
    </div>
    <div>
      Reactive Snapshot With Value Property: {{ favoriteColorControl.value }}
    </div>
    <div>
      Use SetValue To Clear Reactive Form
      <button (click)="clearReactiveForm()">Clear Reactive Form</button>
    </div>
    <form [formGroup]="profileForm" (ngSubmit)="onSubmit()">
      <label>
        First Name:
        <input type="text" formControlName="firstName">
      </label>
      <label>
        Last Name:
        <input type="text" formControlName="lastName">
      </label>
      <button type="submit" [disabled]="!profileForm.valid">Submit</button>
      <button (click)="patchClearForm()">Patch Clear First Name</button>
    </form>
    <form [formGroup]="formBuilderDemoForm">
      <label>
        First Name:
        <input type="text" formControlName="firstName">
      </label>
      <label>
        Last Name:
        <input type="text" formControlName="lastName">
      </label>
      <div formGroupName="address">
        <h3>Address</h3>

        <label>
          Street:
          <input type="text" formControlName="street">
        </label>

        <label>
          City:
          <input type="text" formControlName="city">
        </label>

        <label>
          State (Required With Code):
          <input type="text" formControlName="state">
        </label>

        <label>
          Zip Code (Required With Directive):
          <input type="text" formControlName="zip" required>
        </label>
      </div>
      <div formArrayName="aliases">
        <h3>Aliases</h3>
        <button (click)="addAlias()">Add Alias</button>
        <div *ngFor="let address of aliases.controls; let i=index">
          <label>
            Alias:
            <input type="text" [formControlName]="i">
          </label>
        </div>
      </div>
      <p>
        Form Status: {{formBuilderDemoForm.status}}
      </p>
    </form>
    <div>
      Favorite Color With Template: <input type="text" [(ngModel)]="favoriteColor">
    </div>
    <div class="container">
      <h1>Hero Form</h1>
      <form #heroForm=ngForm>
        <div class="form-group">
          <label for="name">Name</label>
          <input type="text" class="form-control" id="name" required
                 [(ngModel)]="model.name" name="name" 
                 #spy #name="ngModel"
          >
          <div 
            [hidden]="name.valid || name.pristine" class="alert alert-danger"
          >
            Name is required.
          </div>
          <br>TODO: remove this: {{spy.className}}
        </div>
        <div class="form-group">
          <label for="alterEgo">Alter Ego</label>
          <input type="text" class="form-control" id="alterEgo"
                 [(ngModel)]="model.alterEgo" name="alterEgo"
          >
        </div>
        <div class="form-group">
          <label for="power">Hero Power</label>
          <select class="form-control" id="power" 
                  required
                  [(ngModel)]="model.power" name="power"
          >
            <option *ngFor="let pow of powers" [value]="pow">{{pow}}</option>
          </select>
        </div>
        <button type="submit" class="btn btn-success">Submit</button>
        <button type="button" class="btn btn-default" (click)="newHero(); heroForm.reset()">New Hero</button>
      </form>
    </div>
  `,
  styles: [`
    @import url('https://unpkg.com/bootstrap@3.3.7/dist/css/bootstrap.min.css');

    label {
      display: block;
    }
    
    .ng-valid[required], .ng-valid.required {
      border-left: 5px solid
    }
    .ng-invalid:not(form) {
      border-left: 5px solid #a94442;
    }
  `]
})
export class FormOverviewComponent implements OnInit {
  favoriteColorControl = new FormControl('default');
  favoriteColorControlValue$: Observable<string>;
  favoriteColor = '';
  profileForm = new FormGroup({
    firstName: new FormControl(''),
    lastName: new FormControl(''),
  });
  formBuilderDemoForm = this.formBuilder.group({
    firstName: [''],
    lastName: [''],
    address: this.formBuilder.group({
      street: [''],
      city: [''],
      state: ['', Validators.required],
      zip: [''],
    }),
    aliases: this.formBuilder.array([
      this.formBuilder.control('')
    ])
  });

  powers = ['A','B'];
  model = {
    name: '',
    alterEgo: '',
    power: '',
  };

  constructor(private formBuilder: FormBuilder) {
    this.favoriteColorControlValue$ =
      this.favoriteColorControl.valueChanges.pipe(
        map(x => x.toString()),
      );
  }

  ngOnInit() {

  }

  newHero() {
    this.model = {
      name: '',
      alterEgo: '',
      power: ''
    };
  }

  clearReactiveForm() {
    this.favoriteColorControl.setValue('');
  }

  onSubmit() {
    console.info(this.profileForm.value);
  }

  patchClearForm() {
    this.profileForm.patchValue({
      firstName: ''
    });
  }

  addAlias() {
    this.aliases.push(this.formBuilder.control(''));
  }

  get aliases() {
    return this.formBuilderDemoForm.get('aliases') as FormArray;
  }
}
