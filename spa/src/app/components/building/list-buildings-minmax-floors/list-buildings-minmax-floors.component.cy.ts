import { ComponentFixture, TestBed } from '@angular/core/testing'
import { ReactiveFormsModule } from '@angular/forms'
import { of } from 'rxjs'
import {
    BuildingByFloorsDTO,
    BuildingService,
    MinMaxDTO,
} from 'src/app/services/building.service'
import { ListBuildingsMinmaxFloorsComponent } from './list-buildings-minmax-floors.component'

describe('ListBuildingsMinmaxFloorsComponent', () => {
    let buildingServiceStub: Partial<BuildingService>

    let component: ListBuildingsMinmaxFloorsComponent
    let fixture: ComponentFixture<ListBuildingsMinmaxFloorsComponent>

    const buildingsList: BuildingByFloorsDTO[] = [
        {
            code: 'B1',
            name: 'Building 1',
            description: 'Description 1',
            floorNumber: 5,
            maxFloorDimensions: { length: 10, width: 10 },
        },
        {
            code: 'B2',
            name: 'Building 2',
            description: 'Description 2',
            floorNumber: 10,

            maxFloorDimensions: { length: 10, width: 10 },
        },
    ]

    beforeEach(() => {
        buildingServiceStub = {
            getBuildingsByFloors: function (dto: MinMaxDTO) {
                return of(buildingsList)
            },
        }

        TestBed.configureTestingModule({
            declarations: [ListBuildingsMinmaxFloorsComponent],
            imports: [ReactiveFormsModule],
            providers: [{ provide: BuildingService, useValue: buildingServiceStub }],
        })

        fixture = TestBed.createComponent(ListBuildingsMinmaxFloorsComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load buildings on form submission', () => {
        const getBuildingsByFloorsSpy = cy.spy(
            component['service'],
            'getBuildingsByFloors',
        )
        const alertSpy = cy.spy(window, 'alert')

        component.filterForm.setValue({ min: 0, max: 10 })
        component.onSubmit()

        expect(getBuildingsByFloorsSpy).calledWith({ min: 0, max: 10 })
        expect(alertSpy).not.calledOnce
        expect(component.buildings).eq(buildingsList)
    })

    it('should validate form correctly', () => {
        component.filterForm.setValue({ min: 0, max: 10 })
        expect(component.formValid()).to.eq(true)

        component.filterForm.setValue({ min: 5, max: 15 })
        expect(component.formValid()).to.eq(true)

        component.filterForm.setValue({ min: -5, max: 10 })
        expect(component.formValid()).to.eq(false)

        component.filterForm.setValue({ min: 10, max: 5 })
        expect(component.formValid()).to.eq(false)

        component.filterForm.setValue({ min: 0, max: 0 })
        expect(component.formValid()).to.eq(false)
    })
})
