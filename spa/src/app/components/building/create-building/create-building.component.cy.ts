import { ComponentFixture, TestBed } from '@angular/core/testing'
import { FormBuilder, ReactiveFormsModule } from '@angular/forms'
import { Observable, of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateBuildingDTO } from 'src/app/dto/CreateBuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { CreateBuildingComponent } from './create-building.component'

describe('CreateBuildingComponent', () => {
    let buildingServiceStub: Partial<BuildingService>

    let component: CreateBuildingComponent
    let fixture: ComponentFixture<CreateBuildingComponent>

    beforeEach(() => {
        buildingServiceStub = {
            createBuilding: function () {
                return of({
                    code: 'B1',
                    name: 'Building 1',
                    description: 'Description 1',
                    maxFloorDimensions: { length: 10, width: 10 },
                })
            },
        }

        TestBed.configureTestingModule({
            declarations: [CreateBuildingComponent],
            imports: [ReactiveFormsModule],
            providers: [{ provide: BuildingService, useValue: buildingServiceStub }],
        })

        fixture = TestBed.createComponent(CreateBuildingComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should create a building on form submission', () => {
        const createBuildingSpy = cy.spy(component['service'], 'createBuilding')
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.form, 'reset')

        component.form.setValue({
            code: 'B1',
            name: 'Building 1',
            description: 'Description 1',
            length: 10,
            width: 10,
        })

        component.submit()

        expect(createBuildingSpy).calledOnce
        expect(alertSpy).calledWith('Created building B1 - Building 1')
        expect(resetSpy).calledOnce
    })

    it('should reset form on successful building creation', () => {
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.form, 'reset')

        component.form.setValue({
            code: 'B1',
            name: 'Building 1',
            description: 'Description 1',
            length: 10,
            width: 10,
        })

        component.submit()

        expect(alertSpy).calledWith('Created building B1 - Building 1')
        expect(resetSpy).calledOnce
    })
})
