import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TaskMenuComponent } from './task-menu.component';

describe('TaskMenuComponent', () => {
  let component: TaskMenuComponent;
  let fixture: ComponentFixture<TaskMenuComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [TaskMenuComponent]
    });
    fixture = TestBed.createComponent(TaskMenuComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
