import { ComponentFixture, TestBed } from '@angular/core/testing'
import { Observable } from 'rxjs'
import { ClientService } from '../../../services/client.service'
import { CreateClientComponent } from './create-client.component'
import { CreatedClientDTO } from '../../../dto/CreatedClientDTO'

describe('CreateClientComponent: Unit Tests', () => {
    let clientServiceStub: Partial<ClientService>

    let component: CreateClientComponent
    let fixture: ComponentFixture<CreateClientComponent>

    const client1: CreatedClientDTO = {
        name: 'quim',
        email: 'joaxxxo@isep.ipp.pt',
        phoneNumber: '122389763',
        vatNumber: 122389763,
        status: 'Pending',
    }

    beforeEach(() => {
        clientServiceStub = {
            createClient: function () {
                return new Observable<CreatedClientDTO>((observer) => {
                    observer.next(client1)
                    observer.complete()
                })
            },
        }

        TestBed.configureTestingModule({
            declarations: [CreateClientComponent],
            providers: [{ provide: ClientService, useValue: clientServiceStub }],
        })

        fixture = TestBed.createComponent(CreateClientComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should create a client on form submission', () => {
        const createClientSpy = cy.spy(component['service'], 'createClient')
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.form, 'reset')

        component.form.setValue({
            name: 'quim',
            email: 'joaxxxo@isep.ipp.pt',
            phoneNumber: '122389763',
            vatNumber: '122389763',
            password: 'Jonasjonas123!',
            confirmPassword: 'Jonasjonas123!',
        })

        component.isPrivacyPolicyAgreed = true

        component.submit()

        expect(createClientSpy).calledOnce
        expect(alertSpy).calledWith(
            `Created Client: \nName: ${client1.name}\nEmail: ${client1.email}\nPhoneNumber: ${client1.phoneNumber}\nVatNumber: ${client1.vatNumber}`,
        )
        expect(resetSpy).calledOnce
    })

    it('should reset form on successful client creation', () => {
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.form, 'reset')

        component.form.setValue({
            name: 'quim',
            email: 'joaxxxo@isep.ipp.pt',
            phoneNumber: '122389763',
            vatNumber: '122389763',
            password: 'Jonasjonas123!',
            confirmPassword: 'Jonasjonas123!',
        })

        component.isPrivacyPolicyAgreed = true

        component.submit()

        expect(alertSpy).calledWith(
            `Created Client: \nName: ${client1.name}\nEmail: ${client1.email}\nPhoneNumber: ${client1.phoneNumber}\nVatNumber: ${client1.vatNumber}`,
        )
        expect(resetSpy).calledOnce
    })
})
