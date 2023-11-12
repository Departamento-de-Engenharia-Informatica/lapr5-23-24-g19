import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GetBuildingsComponent } from './get-buildings.component';

describe('GetBuildingsComponent', () => {
  let component: GetBuildingsComponent;
  let fixture: ComponentFixture<GetBuildingsComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [GetBuildingsComponent]
    });
    fixture = TestBed.createComponent(GetBuildingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
