import { ComponentFixture, TestBed } from '@angular/core/testing'
import { FormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { ListFloorsComponent } from './list-floors.component'

describe('ListFloorsComponent', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>

    let component: ListFloorsComponent
    let fixture: ComponentFixture<ListFloorsComponent>

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
            description: 'Floor 1',
        },
        {
            buildingCode: 'A',
            floorNumber: 2,
            description: 'Floor 2',
        },
    ]

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

        TestBed.configureTestingModule({
            declarations: [ListFloorsComponent],
            imports: [FormsModule],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
            ],
        })

        fixture = TestBed.createComponent(ListFloorsComponent)
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

    it('should load floors when getFloors is called', () => {
        const buildingCode = 'A'

        component.getFloors(buildingCode)

        expect(component.allFloors).to.eq(floors)
        expect(component.floors).to.eq(floors)
    })
})
