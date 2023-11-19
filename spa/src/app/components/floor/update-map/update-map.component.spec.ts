import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UpdateMapComponent } from './update-map.component';

describe('UpdateMapComponent', () => {
  let component: UpdateMapComponent;
  let fixture: ComponentFixture<UpdateMapComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [UpdateMapComponent]
    });
    fixture = TestBed.createComponent(UpdateMapComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
