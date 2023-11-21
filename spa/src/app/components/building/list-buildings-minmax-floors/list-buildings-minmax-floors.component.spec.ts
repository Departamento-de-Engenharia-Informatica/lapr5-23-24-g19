import { ComponentFixture, TestBed } from '@angular/core/testing'

import { ListBuildingsMinmaxFloorsComponent } from './list-buildings-minmax-floors.component'

describe('ListBuildingsMinmaxFloorsComponent', () => {
    let component: ListBuildingsMinmaxFloorsComponent
    let fixture: ComponentFixture<ListBuildingsMinmaxFloorsComponent>

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [ListBuildingsMinmaxFloorsComponent],
        })
        fixture = TestBed.createComponent(ListBuildingsMinmaxFloorsComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create', () => {
        expect(component).toBeTruthy()
    })
})
