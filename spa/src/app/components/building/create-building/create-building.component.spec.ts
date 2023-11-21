import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateBuildingComponent } from './create-building.component';

describe('CreateBuildingComponent', () => {
  let component: CreateBuildingComponent;
  let fixture: ComponentFixture<CreateBuildingComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [CreateBuildingComponent]
    });
    fixture = TestBed.createComponent(CreateBuildingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
