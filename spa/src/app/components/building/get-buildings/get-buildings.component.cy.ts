import { ComponentFixture, TestBed } from '@angular/core/testing'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { GetBuildingsComponent } from './get-buildings.component'
import { Observable, observable, of } from 'rxjs'

describe('GetBuildingsComponent', () => {
    let buildingServiceStub: Partial<BuildingService>

    let component: GetBuildingsComponent
    let fixture: ComponentFixture<GetBuildingsComponent>

    const buildingsList: BuildingDTO[] = [
        {
            code: 'B1',
            name: 'Building 1',
            description: 'Description 1',
            maxFloorDimensions: { length: 10, width: 10 },
        },
        {
            code: 'B2',
            name: 'Building 2',
            description: 'Description 2',
            maxFloorDimensions: { length: 15, width: 15 },
        },
    ]

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return of(buildingsList)
            },
        }

        TestBed.configureTestingModule({
            declarations: [GetBuildingsComponent],
            providers: [{ provide: BuildingService, useValue: buildingServiceStub }],
        })

        fixture = TestBed.createComponent(GetBuildingsComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load buildings on init', () => {
        const listBuildingsSpy = cy.spy(component, 'listBuildings')

        component.ngOnInit()

        expect(listBuildingsSpy).calledOnce
        expect(component.buildings).eq(buildingsList)
    })
})
