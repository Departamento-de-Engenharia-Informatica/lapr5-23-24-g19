import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreatePassageComponent } from './create-passage.component';

describe('CreatePassageComponent', () => {
  let component: CreatePassageComponent;
  let fixture: ComponentFixture<CreatePassageComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [CreatePassageComponent]
    });
    fixture = TestBed.createComponent(CreatePassageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
