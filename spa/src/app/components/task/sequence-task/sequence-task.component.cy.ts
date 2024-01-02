import { ComponentFixture, TestBed, tick, fakeAsync } from '@angular/core/testing'
import { SequenceTaskComponent } from './sequence-task.component'
import { of } from 'rxjs'
import { TaskService } from 'src/app/services/task.service'
import { RobotSequenceDTO } from 'src/app/dto/RobotSequenceDTO'
import { TaskDTO, TaskState, TaskType } from 'src/app/dto/TaskDTO'

describe('SequenceTaskComponent', () => {
    let component: SequenceTaskComponent
    let fixture: ComponentFixture<SequenceTaskComponent>
    let taskService: Partial<TaskService>

    const tasksMock: TaskDTO[] = [
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
        {
            id: '6cb10517-1ec8-4c90-880d-cb44d23ea8dc',
            state: TaskState.PENDING,
            type: TaskType.SURVEILLANCE,
            requesterEmail: 'zemaenl@isep.ipp.pt',
            requesterName: 'ze ze manel',
            location: {
                startingPoint: { buildingCode: 'A', floorNumber: 1, x: 10, y: 10 },
                endingPoint: { buildingCode: 'A', floorNumber: 1, x: 10, y: 10 },
            },
        },
    ]

    const algorithmsMock: string[] = ['permutations', 'genetic']

    beforeEach(() => {
        taskService = {
            getApprovedTasks: cy.stub().returns(of(tasksMock)),
            taskSequenceAlgorithms: cy.stub().returns(of(algorithmsMock)),
            sequenceTasks: cy.stub().returns(of([])),
        }

        TestBed.configureTestingModule({
            declarations: [SequenceTaskComponent],
            providers: [{ provide: TaskService, useValue: taskService }],
        })

        fixture = TestBed.createComponent(SequenceTaskComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should initialize with approved tasks and sequence algorithms on ngOnInit', fakeAsync(() => {
        fixture.detectChanges()
        tick()

        expect(taskService.taskSequenceAlgorithms).to.be.calledOnce
        expect(taskService.getApprovedTasks).to.be.calledOnce

        expect(component.tasks).to.deep.equal(tasksMock)
        expect(component.algorithms).to.deep.equal(algorithmsMock)
    }))

    it('should sequence tasks successfully', fakeAsync(() => {
        const selectedTaskIds = tasksMock.slice(0, 2).map((t) => t.id)
        const selectedAlgorithm = algorithmsMock[0]

        const expectedSequece: RobotSequenceDTO[] = [
            // {{{
            {
                robotName: 'Alfredo',
                tasks: {
                    cost: 20,
                    initialPosition: { building: 'A', floor: 1, x: 0, y: 1 },
                    order: [
                        {
                            start: {
                                building:
                                    tasksMock[0].location.startingPoint.buildingCode,
                                floor: tasksMock[0].location.startingPoint.floorNumber,
                                x: tasksMock[0].location.startingPoint.x,
                                y: tasksMock[0].location.startingPoint.y,
                            },
                            end: {
                                building: tasksMock[0].location.endingPoint.buildingCode,
                                floor: tasksMock[0].location.endingPoint.floorNumber,
                                x: tasksMock[0].location.endingPoint.x,
                                y: tasksMock[0].location.endingPoint.y,
                            },
                            taskId: tasksMock[0].id,
                        },
                        {
                            start: {
                                building:
                                    tasksMock[1].location.startingPoint.buildingCode,
                                floor: tasksMock[1].location.startingPoint.floorNumber,
                                x: tasksMock[1].location.startingPoint.x,
                                y: tasksMock[1].location.startingPoint.y,
                            },
                            end: {
                                building: tasksMock[1].location.endingPoint.buildingCode,
                                floor: tasksMock[1].location.endingPoint.floorNumber,
                                x: tasksMock[1].location.endingPoint.x,
                                y: tasksMock[1].location.endingPoint.y,
                            },
                            taskId: tasksMock[1].id,
                        },
                    ],
                },
            },
            // }}}
        ]
        const expectedPlanned = tasksMock
            .slice(0, 2)
            .map((t) => ({ id: t.id, type: t.type }))

        taskService.sequenceTasks = cy.stub().returns(of(expectedSequece))

        // Set up the component's tasks and form
        component.selectedTasks = selectedTaskIds
        component.form.get('algorithm')?.setValue(selectedAlgorithm)

        // Intercept the request to sequenceTasks

        expect(component.sequences).to.be.empty

        // Call the sequenceTasks method
        component.sequenceTasks()

        // Wait for the sequenceTasks request to complete
        fixture.detectChanges()
        tick()

        // Ensure that the sequenced tasks are updated in the component
        expect(component.sequences).to.deep.equal([expectedSequece])
        expect(component._planned).to.deep.equal(expectedPlanned)

        expect(taskService.taskSequenceAlgorithms).to.be.calledOnce
        expect(taskService.sequenceTasks).to.be.calledOnceWith({
            algorithm: selectedAlgorithm,
            tasks: expectedPlanned,
        })
        expect(taskService.getApprovedTasks).to.be.calledTwice
    }))
})
