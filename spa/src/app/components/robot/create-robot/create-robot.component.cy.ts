import { ComponentFixture, TestBed } from '@angular/core/testing'
import { FormBuilder, ReactiveFormsModule } from '@angular/forms'
import { Observable, of } from 'rxjs'
import { CreateRobotTypeDTO } from 'src/app/dto/CreateRobotTypeDTO'
import { RobotDTO } from 'src/app/dto/RobotDTO'
import { RobotService } from 'src/app/services/robot.service'
import { CreateRobotComponent } from './create-robot.component'

describe('CreateRobotComponent', () => {
    let robotServiceStub: Partial<RobotService>

    let component: CreateRobotComponent
    let fixture: ComponentFixture<CreateRobotComponent>

    const robotTypes: CreateRobotTypeDTO[] = [
        {
            code: 'R02',
            brand: 'RobotTypeBrandLapr',
            model: 'ModelLapr',
            taskTypes: ['SURVEILLANCE'],
        },
        {
            code: 'R06',
            brand: 'RobotTypeBrandLapr',
            model: 'ModelLapr',
            taskTypes: ['SURVEILLANCE'],
        },
    ]

    const robots: RobotDTO[] = [
        {
            code: 'R1',
            nickname: 'Robot 1',
            typeCode: 'A',
            serialNumber: 'SN001',
            state: 0,
        },
        {
            code: 'R2',
            nickname: 'Robot 2',
            typeCode: 'B',
            serialNumber: 'SN002',
            state: 0,
        },
    ]

    beforeEach(() => {
        robotServiceStub = {
            getRobotTypes: function () {
                return of(robotTypes)
            },
            getRobotsOnion: function () {
                return of(robots)
            },
            createRobot: function () {
                return of(robots[0])
            },
        }

        TestBed.configureTestingModule({
            declarations: [CreateRobotComponent],
            imports: [ReactiveFormsModule],
            providers: [{ provide: RobotService, useValue: robotServiceStub }],
        })

        fixture = TestBed.createComponent(CreateRobotComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load robot types and robots on init', () => {
        component.ngOnInit()
        expect(component.robotTypes).to.eq(robotTypes)
        expect(component.robots).to.eq(robots)
    })

    it('should create a robot on form submission', () => {
        const createRobotSpy = cy.spy(component['svc'], 'createRobot')
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.createRobotForm, 'reset')

        component.createRobotForm.setValue({
            code: 'R3',
            nickname: 'Robot 3',
            typeCode: 'A',
            serialNumber: 'SN003',
            description: '',
        })

        component.onSubmit()

        expect(createRobotSpy).calledWith()
        expect(alertSpy).calledWith(
            'Robot created successfully!\nCode: R1\nNickname: Robot 1\nRobot Type code: A\nSerial Number: SN001',
        )
        expect(resetSpy).calledWith({ description: '' })
    })
})
