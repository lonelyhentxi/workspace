import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { FrameLogoComponent } from './frame-logo.component';

describe('FrameLogoComponent', () => {
  let component: FrameLogoComponent;
  let fixture: ComponentFixture<FrameLogoComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ FrameLogoComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FrameLogoComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
