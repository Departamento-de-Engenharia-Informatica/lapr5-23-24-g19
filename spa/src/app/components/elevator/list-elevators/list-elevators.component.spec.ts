import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListElevatorsComponent } from './list-elevators.component';

describe('ListElevatorsComponent', () => {
  let component: ListElevatorsComponent;
  let fixture: ComponentFixture<ListElevatorsComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ListElevatorsComponent]
    });
    fixture = TestBed.createComponent(ListElevatorsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
