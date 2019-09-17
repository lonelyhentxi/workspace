import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RxjsCh4Component } from './rxjs-ch4.component';

describe('RxjsCh4Component', () => {
  let component: RxjsCh4Component;
  let fixture: ComponentFixture<RxjsCh4Component>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ RxjsCh4Component ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RxjsCh4Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
