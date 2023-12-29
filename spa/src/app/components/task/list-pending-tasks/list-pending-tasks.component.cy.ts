import { ComponentFixture, TestBed } from '@angular/core/testing'
import { of } from 'rxjs'
import { TaskType } from 'src/app/dto/TaskDTO'
import { TaskService } from 'src/app/services/task.service'
import { IGeneralTaskDTO } from '../../../../../../mdr/src/dto/IGeneralTaskDTO'
import { ListPendingTasksComponent } from './list-pending-tasks.component'

describe('ListPendingTasksComponent: Unit Tests', () => {
    let taskServiceStub: Partial<TaskService>

    let component: ListPendingTasksComponent
    let fixture: ComponentFixture<ListPendingTasksComponent>

    const tasks: IGeneralTaskDTO[] = [
        {
            id: { value: '1' },
            email: 'daniel@isep.ipp.pt',
            jobType: 0,
            status: 0,
            location: {
                startingPoint: { buildingCode: 'A', floorNumber: 1, x: 0, y: 0 },
                endingPoint: { buildingCode: 'B', floorNumber: 2, x: 1, y: 1 },
            },
            surveillanceContact: { name: 'Diogo Napoles', phoneNumber: '123456789' },
        },
        {
            id: { value: '2' },
            email: 'daniel@isep.ipp.pt',
            jobType: 1,
            status: 0,
            location: {
                startingPoint: { buildingCode: 'C', floorNumber: 3, x: 1, y: 2 },
                endingPoint: { buildingCode: 'D', floorNumber: 4, x: 3, y: 4 },
            },
            description: 'Dispositivos da Cisco',
            pickupContact: { name: 'Daniel', phoneNumber: '987654321' },
            deliveryContact: { name: 'Joao Teixeira', phoneNumber: '987654321' },
        },
    ]

    beforeEach(() => {
        taskServiceStub = {
            getPendingTasks: function () {
                return of(tasks)
            },
        }

        TestBed.configureTestingModule({
            declarations: [ListPendingTasksComponent],
            providers: [{ provide: TaskService, useValue: taskServiceStub }],
        })

        fixture = TestBed.createComponent(ListPendingTasksComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load pending tasks on init', () => {
        component.ngOnInit()
        expect(component.tasks).to.eq(tasks)
    })

    it('should show details of a surveillance task', () => {
        const alertSpy = cy.spy(window, 'alert')
        const task = tasks[0]
        component.showDetails(task)

        const expectedMessage = `Task ID: ${task.id.value}\nEmail: ${task.email}\nType: ${
            Object.values(TaskType)[task.jobType]
        }\n\nLocation:\n===============\nBuilding: ${
            task.location.startingPoint.buildingCode
        }\nFloor: ${
            task.location.startingPoint.floorNumber
        }\n\nSurveillance Contact:\n===============\nName: ${
            task.surveillanceContact!.name
        }\nPhone: ${task.surveillanceContact!.phoneNumber}\n`

        expect(alertSpy).calledWith(expectedMessage)
    })

    it('should show details of a delivery task', () => {
        const alertSpy = cy.spy(window, 'alert')
        const task = tasks[1]
        component.showDetails(task)

        const expectedMessage = `Task ID: ${task.id.value}\nEmail: ${task.email}\nType: ${
            Object.values(TaskType)[task.jobType]
        }\n\nDescription: ${
            task.description
        }\n\nPickup Location:\n===============\nBuilding: ${
            task.location.startingPoint.buildingCode
        }\nFloor: ${task.location.startingPoint.floorNumber}\nX: ${
            task.location.startingPoint.x
        }\nY: ${
            task.location.startingPoint.y
        }\n\nPickup Contact:\n===============\nName: ${
            task.pickupContact!.name
        }\nPhone: ${
            task.pickupContact!.phoneNumber
        }\n\nDelivery Location:\n===============\nBuilding: ${
            task.location.endingPoint.buildingCode
        }\nFloor: ${task.location.endingPoint.floorNumber}\nX: ${
            task.location.endingPoint.x
        }\nY: ${
            task.location.endingPoint.y
        }\n\nDelivery Contact:\n===============\nName: ${
            task.deliveryContact!.name
        }\nPhone: ${task.deliveryContact!.phoneNumber}\n`

        expect(alertSpy).calledWith(expectedMessage)
    })
})
