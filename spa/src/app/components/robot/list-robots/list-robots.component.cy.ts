import { ComponentFixture, TestBed } from '@angular/core/testing'
import { FormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { ListRobotsComponent } from './list-robots.component'
import { RobotDTO } from '../../../dto/RobotDTO'
import { RobotService } from '../../../services/robot.service'

describe('ListFloorsComponent', () => {
    let robotServiceStub: Partial<RobotService>

    let component: ListRobotsComponent
    let fixture: ComponentFixture<ListRobotsComponent>

    const robots: RobotDTO[] = [
        {
            code: 'R1',
            nickname: 'Robot1',
            typeCode: 'T1',
            serialNumber: 'SN1',
            description: 'Robot 1 Description',
            state: 0,
        },
    ]

    beforeEach(() => {
        robotServiceStub = {
            getRobots: function () {
                return of(robots)
            },
        }

        TestBed.configureTestingModule({
            declarations: [ListRobotsComponent],
            imports: [FormsModule],
            providers: [{ provide: RobotService, useValue: robotServiceStub }],
        })

        fixture = TestBed.createComponent(ListRobotsComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load robots on init', () => {
        component.ngOnInit()

        expect(component.robots).to.eq(robots)
    })
})
