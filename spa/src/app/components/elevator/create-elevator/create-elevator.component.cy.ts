import { ComponentFixture, TestBed } from '@angular/core/testing'
import { Observable } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { ElevatorService } from '../../../services/elevator.service'
import { CreateElevatorComponent } from './create-elevator.component'
import { CreatedElevatorDTO } from '../../../dto/CreatedElevatorDTO'

describe('CreateElevatorComponent: Unit Tests', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>
    let elevatorServiceStub: Partial<ElevatorService>

    let component: CreateElevatorComponent
    let fixture: ComponentFixture<CreateElevatorComponent>

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
        },
        {
            buildingCode: 'A',
            floorNumber: 2,
        },
    ]

    const elevator3: CreatedElevatorDTO = {
        buildingId: 'P',
        identifier: 1,
        floors: [1],
        brand: 'a',
        model: 'a',
        serialNumber: 'a',
        description: 'a',
    }

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return new Observable<BuildingDTO[]>((observer) => {
                    observer.next(buildings)
                    observer.complete()
                })
            },
        }

        floorServiceStub = {
            getFloors: function () {
                return new Observable<FloorAndBuildingDTO[]>((observer) => {
                    observer.next(floors)
                    observer.complete()
                })
            },
        }

        elevatorServiceStub = {
            createElevator: function () {
                return new Observable<CreatedElevatorDTO>((observer) => {
                    observer.next(elevator3)
                    observer.complete()
                })
            },
        }

        TestBed.configureTestingModule({
            declarations: [CreateElevatorComponent],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
                { provide: ElevatorService, useValue: elevatorServiceStub },
            ],
        })

        fixture = TestBed.createComponent(CreateElevatorComponent)
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

    it('should list floors when a building is selected', () => {
        const selectedBuildingCode = 'A'

        component.selectedBuilding = selectedBuildingCode
        component.listFloors()

        expect(component.floors).to.eq(floors)
    })

    it('should reset form and show success alert on successful elevator creation', () => {
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.createElevatorForm, 'reset')

        component.onSubmit()

        expect(alertSpy).calledWith(`Elevator created successfully!\n`)
    })
})
