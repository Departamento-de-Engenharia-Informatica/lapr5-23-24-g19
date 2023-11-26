import { ComponentFixture, TestBed } from '@angular/core/testing'
import { ReactiveFormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { EditFloorComponent } from './edit-floor.component'

describe('EditFloorComponent', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>

    let component: EditFloorComponent
    let fixture: ComponentFixture<EditFloorComponent>

    const buildings: BuildingDTO[] = [
        {
            code: 'A',
            maxFloorDimensions: { length: 10, width: 10 },
        },
        {
            code: 'B',
            maxFloorDimensions: { length: 10, width: 10 },
        },
    ]

    const floors: FloorAndBuildingDTO[] = [
        {
            buildingCode: 'A',
            floorNumber: 1,
            description: 'Floor 1',
        },
        {
            buildingCode: 'A',
            floorNumber: 2,
            description: 'Floor 2',
        },
    ]

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return of(buildings)
            },
        }

        floorServiceStub = {
            getFloors: function () {
                return of(floors)
            },
            putFloor: function () {
                return of(floors[0])
            },
            patchFloor: function () {
                return of(floors[0])
            },
        }

        TestBed.configureTestingModule({
            declarations: [EditFloorComponent],
            imports: [ReactiveFormsModule],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
            ],
        })

        fixture = TestBed.createComponent(EditFloorComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load buildings on init', () => {
        component.ngOnInit()

        expect(component.buildings).to.eq(buildings)
    })

    it('should load floors when a building is selected', () => {
        component.selectedBuilding = 'A'

        component.getFloors()

        expect(component.floors).to.eq(floors)
    })

    it('should submit the form with override and call putFloor', () => {
        const putFloorSpy = cy.spy(component['floorService'], 'putFloor')

        component.editFloorForm.setValue({
            buildingCode: 'A',
            oldFloorNumber: 1,
            newFloorNumber: 3,
            newDescription: 'Updated Floor',
            override: true,
        })

        component.onSubmit()

        expect(putFloorSpy).calledOnce
    })

    it('should submit the form without override and call patchFloor', () => {
        const patchFloorSpy = cy.spy(component['floorService'], 'patchFloor')

        component.editFloorForm.setValue({
            buildingCode: 'A',
            oldFloorNumber: 1,
            newFloorNumber: 3,
            newDescription: 'Updated Floor',
            override: false,
        })

        component.onSubmit()

        expect(patchFloorSpy).calledOnce
    })

    it('should reset the form and show success alert on successful floor update', () => {
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.editFloorForm, 'reset')
        component.onSubmit()
        expect(alertSpy).calledWith(
            'Floor edited successfully!\nFloor number: 1\nDescription: Floor 1',
        )
        expect(resetSpy).calledWith({
            buildingCode: component.selectedBuilding,
            override: false,
            newDescription: '',
        })
    })
})
