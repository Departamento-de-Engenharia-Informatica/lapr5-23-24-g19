import { ComponentFixture, TestBed } from '@angular/core/testing'
import { Observable } from 'rxjs'

import { DeleteClientComponent } from './delete-client.component'
import { ClientService } from '../../../services/client.service'
import { ClientEmailDTO } from '../../../dto/ClientEmailDTO'
import { IClientWithoutPasswordDTO } from '../../../dto/IClientWithoutPasswordDTO'
import { AuthService } from '@auth0/auth0-angular'

describe('DeleteClientComponent: Unit Tests', () => {
    let clientServiceStub: Partial<ClientService>

    let authServiceStub: Partial<AuthService>

    let component: DeleteClientComponent
    let fixture: ComponentFixture<DeleteClientComponent>

    const dtoEmailClient: ClientEmailDTO = {
        email: 'joaquimfontexxxxsxtxo@isep.ipp.pt',
    }

    const client: IClientWithoutPasswordDTO = {
        name: 'quim',
        email: 'joaquimfontexxxxsxtxo@isep.ipp.pt',
        phoneNumber: '122319763',
        vatNumber: 122389163,
    }

    beforeEach(() => {
        authServiceStub = {
            isAuthenticated$: new Observable((subscriber) => {
                subscriber.next(true)
                subscriber.complete()
            }),
            user$: new Observable((subscriber) => {
                subscriber.next({ email: 'joaquimfontexxxxsxtxo@isep.ipp.pt' })
                subscriber.complete()
            }),
        }

        clientServiceStub = {
            getClient: function () {
                return new Observable<IClientWithoutPasswordDTO>((observer) => {
                    observer.next(client)
                    observer.complete()
                })
            },

            deleteClient: function () {
                return new Observable<ClientEmailDTO>((observer) => {
                    observer.next(dtoEmailClient)
                    observer.complete()
                })
            },
        }

        TestBed.configureTestingModule({
            declarations: [DeleteClientComponent],
            providers: [
                { provide: ClientService, useValue: clientServiceStub },
                { provide: AuthService, useValue: authServiceStub },
            ],
        })

        fixture = TestBed.createComponent(DeleteClientComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should load client on init', () => {
        component.ngOnInit()

        expect(component.client).to.eq(client)
    })

    it('should delete client on submit', () => {
        const deleteClientSpy = cy.spy(component['clientService'], 'deleteClient')
        const alertSpy = cy.spy(window, 'alert')

        component.onSubmit()

        expect(deleteClientSpy).calledOnce
        expect(alertSpy).calledWith(`Deleted Account \nEmail: ${dtoEmailClient.email}`)
    })
})
