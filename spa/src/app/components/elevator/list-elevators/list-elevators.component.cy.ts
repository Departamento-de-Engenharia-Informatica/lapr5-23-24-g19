import { ComponentFixture, TestBed } from '@angular/core/testing'
import { FormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { ElevatorService } from '../../../services/elevator.service'
import { CreatedElevatorDTO } from '../../../dto/CreatedElevatorDTO'
import { ListElevatorsComponent } from './list-elevators.component'

describe('ListElevatorsComponent', () => {
    let buildingServiceStub: Partial<BuildingService>
    let elevatorServiceStub: Partial<ElevatorService>

    let component: ListElevatorsComponent
    let fixture: ComponentFixture<ListElevatorsComponent>

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

    const elevators: CreatedElevatorDTO[] = [
        {
            buildingId: 'P',
            identifier: 1,
            floors: [1],
            brand: 'a',
            model: 'a',
            serialNumber: 'a',
            description: 'a',
        },
    ]

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return of(buildings)
            },
        }

        elevatorServiceStub = {
            getElevators: function () {
                return of(elevators)
            },
        }

        TestBed.configureTestingModule({
            declarations: [ListElevatorsComponent],
            imports: [FormsModule],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: ElevatorService, useValue: elevatorServiceStub },
            ],
        })

        fixture = TestBed.createComponent(ListElevatorsComponent)
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

    it('should load elevators when getElevators is called', () => {
        component.getElevators()

        expect(component.allElevators).to.eq(elevators)
        expect(component.elevators).to.eq(elevators)
    })
})
