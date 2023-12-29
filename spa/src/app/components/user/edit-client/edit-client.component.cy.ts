import { ComponentFixture, TestBed, tick, fakeAsync } from '@angular/core/testing'
import { ReactiveFormsModule } from '@angular/forms'
import { of } from 'rxjs'
import { AuthService } from '@auth0/auth0-angular'
import { EditClientComponent } from './edit-client.component'
import { ClientService } from 'src/app/services/client.service'
import { IClientWithoutPasswordDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'

describe('EditClientComponent', () => {
    let component: EditClientComponent
    let fixture: ComponentFixture<EditClientComponent>
    let authService: Partial<AuthService>
    let clientService: Partial<ClientService>

    const clientMock: IClientWithoutPasswordDTO = {
        email: 'carlos@isep.ipp.pt',
        name: 'Carlos Da Maia',
        phoneNumber: '123456789',
        vatNumber: 123456789,
    }

    beforeEach(() => {
        authService = {
            isAuthenticated$: of(true),
            user$: of({ email: 'carlos@isep.ipp.pt' }),
        }

        clientService = {
            getClient: () => of(clientMock),
            patchClient: () => of(clientMock),
        }

        TestBed.configureTestingModule({
            imports: [ReactiveFormsModule],
            declarations: [EditClientComponent],
            providers: [
                { provide: AuthService, useValue: authService },
                { provide: ClientService, useValue: clientService },
            ],
        })

        fixture = TestBed.createComponent(EditClientComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should initialize with user data on ngOnInit', fakeAsync(() => {
        fixture.detectChanges()
        tick()
        expect(component.email).to.equal('carlos@isep.ipp.pt')
        expect(component.client).to.deep.equal(clientMock)
    }))

    it('should set emailExists to true when email is not empty', () => {
        component.email = 'carlos@isep.ipp.pt'
        expect(component.emailExists()).to.be.true
    })

    it('should set emailExists to false when email is empty', () => {
        component.email = ''
        expect(component.emailExists()).to.be.false
    })

    it('should call getClient on ngOnInit', fakeAsync(() => {
        const spy = cy.spy(clientService, 'getClient')

        component.ngOnInit()

        tick()
        expect(spy).to.be.calledOnce
    }))

    it('should call patchClient on onSubmit', fakeAsync(() => {
        const spy = cy.spy(clientService, 'patchClient')

        component.editClientForm.setValue({
            name: 'Ricardo Ferreira',
            phoneNumber: '987654321',
            vatNumber: '987654321',
        })

        component.onSubmit()

        tick()
        expect(spy).to.have.been.calledWith({
            email: 'carlos@isep.ipp.pt',
            name: 'Ricardo Ferreira',
            phoneNumber: '987654321',
            vatNumber: '987654321',
        })
    }))
})
