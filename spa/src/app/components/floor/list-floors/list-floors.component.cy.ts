import { ComponentFixture, TestBed } from '@angular/core/testing'

import { ListFloorsComponent } from './list-floors.component'

describe('ListFloorsComponent', () => {
    let component: ListFloorsComponent
    let fixture: ComponentFixture<ListFloorsComponent>

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [ListFloorsComponent],
        })
        fixture = TestBed.createComponent(ListFloorsComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create', () => {
        expect(component).toBeTruthy()
    })
})
