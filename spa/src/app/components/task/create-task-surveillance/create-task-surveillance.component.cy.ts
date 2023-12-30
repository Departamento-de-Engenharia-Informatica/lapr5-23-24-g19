import { ComponentFixture, TestBed } from '@angular/core/testing'
import { Observable, of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateSurveillanceTaskDTO } from '../../../../../../mdr/src/dto/CreateSurveillanceTaskDTO'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { BuildingService } from 'src/app/services/building.service'
import { TaskService } from 'src/app/services/task.service'
import { AuthService } from '@auth0/auth0-angular'
import { CreateTaskSurveillanceComponent } from './create-task-surveillance.component'
import { ReactiveFormsModule } from '@angular/forms'

describe('CreateTaskSurveillanceComponent: Unit Tests', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>
    let taskServiceStub: Partial<TaskService>
    let authServiceStub: Partial<AuthService>

    let component: CreateTaskSurveillanceComponent
    let fixture: ComponentFixture<CreateTaskSurveillanceComponent>

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

    const task: CreateSurveillanceTaskDTO = {
        email: 'diogo@isep.ipp.pt',
        buildingCode: 'A',
        floorNumber: 1,
        contactName: 'Diogo Napoles',
        contactPhone: '998123123',
    }

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
        }

        taskServiceStub = {
            createSurveillanceTask: function () {
                return of(task)
            },
        }

        authServiceStub = {
            isAuthenticated$: of(true),
            user$: of({ email: 'diogo@isep.ipp.pt' }),
        }

        TestBed.configureTestingModule({
            imports: [ReactiveFormsModule],
            declarations: [CreateTaskSurveillanceComponent],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
                { provide: TaskService, useValue: taskServiceStub },
                { provide: AuthService, useValue: authServiceStub },
            ],
        })

        fixture = TestBed.createComponent(CreateTaskSurveillanceComponent)
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

    it('should reset form and show success alert on successful task creation', () => {
        const resetSpy = cy.spy(component.createSurveillanceForm, 'reset')

        component.onSubmit()

        expect(resetSpy).calledWith({
            email: 'diogo@isep.ipp.pt',
        })
    })
})
