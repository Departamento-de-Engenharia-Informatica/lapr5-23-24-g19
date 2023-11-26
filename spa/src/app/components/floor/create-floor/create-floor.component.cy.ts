import { ComponentFixture, TestBed } from '@angular/core/testing'
import { Observable } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { CreateFloorComponent } from './create-floor.component'

describe('CreateFloorComponent: Unit Tests', () => {
    let buildingServiceStub: Partial<BuildingService>
    let floorServiceStub: Partial<FloorService>

    let component: CreateFloorComponent
    let fixture: ComponentFixture<CreateFloorComponent>

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
        },
        {
            buildingCode: 'A',
            floorNumber: 2,
        },
    ]

    const floor3: FloorAndBuildingDTO = {
        buildingCode: 'A',
        floorNumber: 3,
    }

    beforeEach(() => {
        buildingServiceStub = {
            getBuildings: function () {
                return new Observable<BuildingDTO[]>((observer) => {
                    observer.next(buildings)
                    observer.complete()
                })
            },
        }

        floorServiceStub = {
            getFloors: function () {
                return new Observable<FloorAndBuildingDTO[]>((observer) => {
                    observer.next(floors)
                    observer.complete()
                })
            },

            createFloor: function () {
                return new Observable<FloorAndBuildingDTO>((observer) => {
                    observer.next(floor3)
                    observer.complete()
                })
            },
        }

        TestBed.configureTestingModule({
            declarations: [CreateFloorComponent],
            providers: [
                { provide: BuildingService, useValue: buildingServiceStub },
                { provide: FloorService, useValue: floorServiceStub },
            ],
        })

        fixture = TestBed.createComponent(CreateFloorComponent)
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

    it('should list floors when a building is selected', () => {
        const selectedBuildingCode = 'A'

        component.selectedBuilding = selectedBuildingCode
        component.listFloors()

        expect(component.floors).to.eq(floors)
    })

    it('should reset form and show success alert on successful floor creation', () => {
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.createFloorForm, 'reset')

        component.onSubmit()

        expect(alertSpy).calledWith(
            `Floor created successfully!\nFloor number: ${floor3.floorNumber}`,
        )

        expect(resetSpy).calledWith({
            buildingCode: component.selectedBuilding,
            description: '',
        })
    })
})
