import { ComponentFixture, TestBed } from '@angular/core/testing'
import { ReactiveFormsModule } from '@angular/forms'
import { AuthService } from '@auth0/auth0-angular'
import { of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateDeliveryTaskDTO } from '../../../../../../mdr/src/dto/CreateDeliveryTaskDTO'
import { CreatedRoomDTO } from 'src/app/dto/CreatedRoomDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { RoomService } from 'src/app/services/room.service'
import { TaskService } from 'src/app/services/task.service'
import { CreateTaskDeliveryComponent } from './create-task-delivery.component'

describe('CreateTaskDeliveryComponent: Unit Tests', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>
    let roomServiceStub: Partial<RoomService>
    let taskServiceStub: Partial<TaskService>
    let authServiceStub: Partial<AuthService>

    let component: CreateTaskDeliveryComponent
    let fixture: ComponentFixture<CreateTaskDeliveryComponent>

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

    const floors1: FloorAndBuildingDTO[] = [
        {
            buildingCode: 'A',
            floorNumber: 1,
        },
        {
            buildingCode: 'A',
            floorNumber: 2,
        },
    ]

    const rooms1: CreatedRoomDTO[] = [
        {
            buildingCode: 'A',
            floorNumber: 1,
            name: 'A101',
            description: 'Room 1',
            category: 'ROOM',
            positions: { x: 0, y: 0 },
            dimensions: { length: 10, width: 10 },
        },
        {
            buildingCode: 'A',
            floorNumber: 1,
            name: 'A102',
            description: 'Room 2',
            category: 'ROOM',
            positions: { x: 0, y: 0 },
            dimensions: { length: 10, width: 10 },
        },
    ]

    const task: CreateDeliveryTaskDTO = {
        email: 'diogo@isep.ipp.pt',
        startBuildingCode: 'A',
        startFloorNumber: 1,
        startRoom: 'A101',
        goalBuildingCode: 'B',
        goalFloorNumber: 1,
        goalRoom: 'B101',
        pickupContactName: 'Diogo Napoles',
        pickupContactPhone: '998123123',
        deliveryContactName: 'Jonas Trindade',
        deliveryContactPhone: '987654321',
        confirmationCode: 123456,
    }

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return of(buildings)
            },
        }

        floorServiceStub = {
            getFloors: function () {
                return of(floors1)
            },
        }

        roomServiceStub = {
            getRooms: function () {
                return of(rooms1)
            },
        }

        taskServiceStub = {
            createDeliveryTask: function () {
                return of(task)
            },
        }

        authServiceStub = {
            isAuthenticated$: of(true),
            user$: of({ email: 'diogo@isep.ipp.pt' }),
        }

        TestBed.configureTestingModule({
            imports: [ReactiveFormsModule],
            declarations: [CreateTaskDeliveryComponent],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
                { provide: RoomService, useValue: roomServiceStub },
                { provide: TaskService, useValue: taskServiceStub },
                { provide: AuthService, useValue: authServiceStub },
            ],
        })

        fixture = TestBed.createComponent(CreateTaskDeliveryComponent)
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

    it('should list floors when building is selected', () => {
        const selectedBuildingCode = 'A'
        component.selectedBuilding1 = selectedBuildingCode

        component.listFloors1()
        expect(component.floors1).to.eq(floors1)
    })

    it('should reset form and show success alert on successful task creation', () => {
        const resetSpy = cy.spy(component.createDeliveryForm, 'reset')

        component.onSubmit()

        expect(resetSpy).to.be.called
        expect(component.createDeliveryForm.value.email).to.eq('diogo@isep.ipp.pt')
    })
})
