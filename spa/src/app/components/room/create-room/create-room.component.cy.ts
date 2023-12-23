import { ComponentFixture, TestBed } from '@angular/core/testing'
import { Observable } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { ElevatorService } from '../../../services/elevator.service'
import { CreatedElevatorDTO } from '../../../dto/CreatedElevatorDTO'
import { RoomService } from '../../../services/room.service'
import { CreateRoomComponent } from './create-room.component'
import { CreatedRoomDTO } from '../../../dto/CreatedRoomDTO'

describe('CreateRoomComponent: Unit Tests', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>
    let roomServiceStub: Partial<RoomService>

    let component: CreateRoomComponent
    let fixture: ComponentFixture<CreateRoomComponent>

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

    const room3: CreatedRoomDTO = {
        name: 'a',
        buildingCode: 'A',
        floorNumber: 1,
        description: 'desc',
        category: 'GABINETE',
        dimensions: { length: 10, width: 10 },
        positions: {
            x: 2,
            y: 3,
        },
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

        roomServiceStub = {
            createRoom: function () {
                return new Observable<CreatedRoomDTO>((observer) => {
                    observer.next(room3)
                    observer.complete()
                })
            },
        }

        TestBed.configureTestingModule({
            declarations: [CreateRoomComponent],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
                { provide: RoomService, useValue: roomServiceStub },
            ],
        })

        fixture = TestBed.createComponent(CreateRoomComponent)
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

    it('should reset form and show success alert on successful room creation', () => {
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.createRoomForm, 'reset')

        component.onSubmit()

        expect(alertSpy).calledWith(`Room created successfully!\n`)
    })
})
