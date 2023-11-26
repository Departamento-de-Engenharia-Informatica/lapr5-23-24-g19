import { ComponentFixture, TestBed } from '@angular/core/testing'
import { FormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorPassageDTO, FloorService } from 'src/app/services/floor.service'
import { ListFloorsWithPassageComponent } from './list-floors-with-passage.component'

describe('ListFloorsWithPassageComponent', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>

    let component: ListFloorsWithPassageComponent
    let fixture: ComponentFixture<ListFloorsWithPassageComponent>

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

    const floorPassages: FloorPassageDTO[] = [
        {
            floor: {
                buildingCode: 'A',
                floorNumber: 1,
                description: 'Floor 1',
            },
            passage: {
                passageId: 1,
                passageDescription: 'Passage 1',
            },
        },
        {
            floor: {
                buildingCode: 'A',
                floorNumber: 2,
                description: 'Floor 2',
            },
            passage: {
                passageId: 2,
                passageDescription: 'Passage 2',
            },
        },
    ]

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return of(buildings)
            },
        }

        floorServiceStub = {
            getFloorsWithPassage: function () {
                return of(floorPassages)
            },
        }

        TestBed.configureTestingModule({
            declarations: [ListFloorsWithPassageComponent],
            imports: [FormsModule],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
            ],
        })

        fixture = TestBed.createComponent(ListFloorsWithPassageComponent)
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

    it('should load floors with passages when getFloorsWithPassage is called', () => {
        const buildingCode = 'A'
        component.selectedBuilding = buildingCode
        component.getFloorsWithPassage()
        expect(component.allFloorsPassages).to.eq(floorPassages)
        expect(component.floorPassages).to.eq(floorPassages)
    })
})
