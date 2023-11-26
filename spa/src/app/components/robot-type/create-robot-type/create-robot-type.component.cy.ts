import { ComponentFixture, TestBed } from '@angular/core/testing'
import { ReactiveFormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { TaskTypeDTO } from 'src/app/dto/CreateRobotTypeDTO'
import { RobotTypeService } from 'src/app/services/robot-type.service'
import { TaskService } from 'src/app/services/task.service'
import { CreateRobotTypeComponent } from './create-robot-type.component'

describe('CreateRobotTypeComponent', () => {
    let robotTypeServiceStub: Partial<RobotTypeService>
    let taskServiceStub: Partial<TaskService>

    let component: CreateRobotTypeComponent
    let fixture: ComponentFixture<CreateRobotTypeComponent>

    const taskTypes: TaskTypeDTO[] = [
        { description: 'TaskType1' },
        { description: 'TaskType2' },
        { description: 'TaskType3' },
    ]

    beforeEach(() => {
        robotTypeServiceStub = {
            createRobotType: function () {
                return of('Robot type created successfully')
            },
        }

        taskServiceStub = {
            tasksTypes: function () {
                return of(taskTypes)
            },
        }

        TestBed.configureTestingModule({
            declarations: [CreateRobotTypeComponent],
            imports: [ReactiveFormsModule],
            providers: [
                { provide: RobotTypeService, useValue: robotTypeServiceStub },
                { provide: TaskService, useValue: taskServiceStub },
            ],
        })

        fixture = TestBed.createComponent(CreateRobotTypeComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load task types on init', () => {
        component.ngOnInit()
        expect(component.taskTypes.length).to.eq(3)
    })

    it('should create a robot type on form submission', () => {
        const createRobotTypeSpy = cy.spy(
            component['robotTypeService'],
            'createRobotType',
        )
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.createRobotTypeForm, 'reset')

        component.createRobotTypeForm.setValue({
            code: 'RT1',
            brand: 'Brand1',
            model: 'Model1',
            taskTypes: ['TaskType1', 'TaskType2'],
        })

        component.onSubmit()

        expect(createRobotTypeSpy).calledOnce
        expect(alertSpy).calledWith('Robot type created successfully')
        expect(resetSpy).calledOnce
    })
})
