import { ComponentFixture, TestBed } from '@angular/core/testing'

import { ListFloorsWithPassage } from './list-floorsWithPassage'

describe('list-floors-with-passage', () => {
    let component: ListFloorsWithPassage
    let fixture: ComponentFixture<ListFloorsWithPassage>

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [ListFloorsWithPassage],
        })
        fixture = TestBed.createComponent(ListFloorsWithPassage)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create', () => {
        expect(component).toBeTruthy()
    })
})
