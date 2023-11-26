import { ComponentFixture, TestBed } from '@angular/core/testing'

import { CreateBuildingComponent } from './create-building.component'
import { BuildingService } from 'src/app/services/building.service'
import { HttpClient, HttpHandler } from '@angular/common/http'

describe('CreateBuildingComponent', () => {
    let component: CreateBuildingComponent
    let fixture: ComponentFixture<CreateBuildingComponent>

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [CreateBuildingComponent],
            providers: [BuildingService, HttpClient, HttpHandler],
        })
        fixture = TestBed.createComponent(CreateBuildingComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create', () => {
        expect(component).to.exist
    })
})
