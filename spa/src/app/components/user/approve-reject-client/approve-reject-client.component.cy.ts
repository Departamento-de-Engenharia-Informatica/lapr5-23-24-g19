import { ComponentFixture, TestBed, tick, fakeAsync } from '@angular/core/testing'
import { ApproveRejectClientComponent } from './approve-reject-client.component'
import { of } from 'rxjs'
import { ClientService } from 'src/app/services/client.service'
import { IClientWithoutPasswordDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'
import IUpdateClientStateDTO from '../../../../../../mdr/src/dto/IUpdateClientStateDTO'

describe('ApproveRejectClientComponent', () => {
    let component: ApproveRejectClientComponent
    let fixture: ComponentFixture<ApproveRejectClientComponent>
    let clientService: Partial<ClientService>

    const clientsMock: IClientWithoutPasswordDTO[] = [
        { email: 'ricardo@isep.ipp.pt', name: 'Ricardo Reis' },
        { email: 'alberto@isep.ipp.pt', name: 'Alberto Caeiro' },
    ]

    beforeEach(() => {
        clientService = {
            getPendingClients: cy.stub().returns(of(clientsMock)),
            updateClientState: cy.stub().returns(of(null)),
        }

        TestBed.configureTestingModule({
            declarations: [ApproveRejectClientComponent],
            providers: [{ provide: ClientService, useValue: clientService }],
        })

        fixture = TestBed.createComponent(ApproveRejectClientComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should initialize with clients on ngOnInit', fakeAsync(() => {
        fixture.detectChanges()
        tick()
        expect(component.clients).to.deep.equal(clientsMock)
    }))

    it('should call getPendingClients on ngOnInit', fakeAsync(() => {
        component.ngOnInit()
        tick()
        expect(clientService.getPendingClients).to.be.calledTwice
    }))

    it('should call updateClientState with "rejected" on rejectClient', fakeAsync(() => {
        const email = 'ricardo@isep.ipp.pt'
        component.rejectClient(email)
        tick()
        expect(clientService.updateClientState).to.be.calledWith({
            email,
            state: 'rejected',
        } as IUpdateClientStateDTO)
    }))

    it('should call updateClientState with "approved" on approveClient', fakeAsync(() => {
        const email = 'alberto@isep.ipp.pt'
        component.approveClient(email)
        tick()
        expect(clientService.updateClientState).to.be.calledWith({
            email,
            state: 'approved',
        } as IUpdateClientStateDTO)
    }))

    it('should call getPendingClients after updateClientState', fakeAsync(() => {
        const email = 'ricardo@isep.ipp.pt'
        component.approveClient(email)
        tick()
        expect(clientService.getPendingClients).to.be.calledTwice
    }))
})
