import { ComponentFixture, TestBed } from '@angular/core/testing'
import { FormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { ListPassagesBetweenBuildingsComponent } from './list-passages-between-buildings.component'
import { PassageService } from '../../../services/passage.service'
import { PassageDTO } from '../../../dto/PassageDTO'

describe('ListPassagesBetweenBuildingsComponent', () => {
    let buildingServiceStub: Partial<BuildingService>
    let passageServiceStub: Partial<PassageService>

    let component: ListPassagesBetweenBuildingsComponent
    let fixture: ComponentFixture<ListPassagesBetweenBuildingsComponent>

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

    const passages: PassageDTO[] = [
        {
            floor1: {
                buildingCode: 'O',
                floorNumber: 2,
            },
            floor2: {
                buildingCode: 'P',
                floorNumber: 3,
            },
        },
    ]

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return of(buildings)
            },
        }

        passageServiceStub = {
            getPassagesBetweenBuildings: function () {
                return of(passages)
            },
        }

        TestBed.configureTestingModule({
            declarations: [ListPassagesBetweenBuildingsComponent],
            imports: [FormsModule],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: PassageService, useValue: passageServiceStub },
            ],
        })

        fixture = TestBed.createComponent(ListPassagesBetweenBuildingsComponent)
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

    it('should load elevators when getPassagesBetweenBuildings is called', () => {
        component.getPassagesBetweenBuildings()

        expect(component.allPassages).to.eq(passages)
        expect(component.passages).to.eq(passages)
    })
})
