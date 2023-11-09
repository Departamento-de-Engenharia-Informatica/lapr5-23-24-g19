import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RobotTypeComponent } from './robot-type.component';

describe('RobotTypeComponent', () => {
  let component: RobotTypeComponent;
  let fixture: ComponentFixture<RobotTypeComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [RobotTypeComponent]
    });
    fixture = TestBed.createComponent(RobotTypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
