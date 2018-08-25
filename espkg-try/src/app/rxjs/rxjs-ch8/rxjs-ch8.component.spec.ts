import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RxjsCh8Component } from './rxjs-ch8.component';

describe('RxjsCh8Component', () => {
  let component: RxjsCh8Component;
  let fixture: ComponentFixture<RxjsCh8Component>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ RxjsCh8Component ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RxjsCh8Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
