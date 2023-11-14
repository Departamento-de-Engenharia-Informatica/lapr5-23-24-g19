import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EditFloorComponent } from './edit-floor.component';

describe('EditFloorComponent', () => {
  let component: EditFloorComponent;
  let fixture: ComponentFixture<EditFloorComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [EditFloorComponent]
    });
    fixture = TestBed.createComponent(EditFloorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
