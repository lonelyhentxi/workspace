import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RxjsCh5Component } from './rxjs-ch5.component';

describe('RxjsCh5Component', () => {
  let component: RxjsCh5Component;
  let fixture: ComponentFixture<RxjsCh5Component>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ RxjsCh5Component ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RxjsCh5Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
