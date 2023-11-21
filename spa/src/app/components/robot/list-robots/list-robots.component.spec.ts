import { ComponentFixture, TestBed } from '@angular/core/testing'

import { ListRobotsComponent } from './list-robots.component'

describe('ListRobots', () => {
    let component: ListRobotsComponent
    let fixture: ComponentFixture<ListRobotsComponent>

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [ListRobotsComponent],
        })
        fixture = TestBed.createComponent(ListRobotsComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create', () => {
        expect(component).toBeTruthy()
    })
})
