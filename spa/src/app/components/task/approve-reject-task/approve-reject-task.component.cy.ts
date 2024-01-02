import { ComponentFixture, TestBed, tick, fakeAsync } from '@angular/core/testing'
import { ApproveRejectTaskComponent } from './approve-reject-task.component'
import { of } from 'rxjs'
import { TaskService } from 'src/app/services/task.service'
import { TaskDTO, TaskState, TaskType } from 'src/app/dto/TaskDTO'
import { UpdateTaskDTO } from 'src/app/dto/UpdateTaskDTO'

describe('ApproveRejectTaskComponent', () => {
    let component: ApproveRejectTaskComponent
    let fixture: ComponentFixture<ApproveRejectTaskComponent>
    let taskService: Partial<TaskService>

    const tasksMock: Partial<TaskDTO>[] = [
        {
            id: 'c4dcdd97-d36f-4f0e-8c4f-3817bbca121f',
            state: TaskState.PENDING,
            type: TaskType.DELIVERY,
            location: {
                startingPoint: { buildingCode: 'A', floorNumber: 1, x: 0, y: 1 },
                endingPoint: { buildingCode: 'B', floorNumber: 2, x: 14, y: 3 },
            },
            requesterEmail: 'user@isep.ipp.pt',
            requesterName: 'John Doe',
        },
        {
            id: 'b3b465cf-f1c4-4f6a-9f10-56f6035d0599',
            state: TaskState.PENDING,
            type: TaskType.SURVEILLANCE,
            requesterEmail: 'anotheruser@isep.ipp.pt',
            requesterName: 'Jane Doe',
            location: {
                startingPoint: { buildingCode: 'C', floorNumber: 3, x: 15, y: 11 },
                endingPoint: { buildingCode: 'C', floorNumber: 3, x: 15, y: 11 },
            },
        },
    ]

    beforeEach(() => {
        taskService = {
            tasksOfState: cy.stub().returns(of(tasksMock)),
            updateTask: cy.stub().returns(of(null)),
        }

        TestBed.configureTestingModule({
            declarations: [ApproveRejectTaskComponent],
            providers: [{ provide: TaskService, useValue: taskService }],
        })

        fixture = TestBed.createComponent(ApproveRejectTaskComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should initialize with tasks on ngOnInit', fakeAsync(() => {
        fixture.detectChanges()
        tick()
        expect(component.tasks).to.deep.equal(tasksMock)
    }))

    it('should call tasksOfState on ngOnInit', fakeAsync(() => {
        component.ngOnInit()
        tick()
        expect(taskService.tasksOfState).to.be.calledWith(TaskState.PENDING)
    }))

    it('should call updateTask with "rejected" on rejectTask', fakeAsync(() => {
        const taskId = 'c4dcdd97-d36f-4f0e-8c4f-3817bbca121f'
        component.rejectTask(taskId)
        tick()
        expect(taskService.updateTask).to.be.calledWith({
            id: taskId,
            taskStatus: TaskState.REJECTED,
        } as UpdateTaskDTO)
    }))

    it('should call updateTask with "approved" on approveTask', fakeAsync(() => {
        const taskId = 'b3b465cf-f1c4-4f6a-9f10-56f6035d0599'
        component.approveTask(taskId)
        tick()
        expect(taskService.updateTask).to.be.calledWith({
            id: taskId,
            taskStatus: TaskState.APPROVED,
        } as UpdateTaskDTO)
    }))

    it('should remove task after updateTask', fakeAsync(() => {
        const taskId = 'c4dcdd97-d36f-4f0e-8c4f-3817bbca121f'
        component.approveTask(taskId)
        tick()
        expect(component.tasks.some((t) => t.id === taskId)).to.be.false
    }))
})
