import { Component } from '@angular/core'
import { IClientWithoutPasswordDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'
import { ClientService } from 'src/app/services/client.service'
import IUpdateClientStateDTO from '../../../../../../mdr/src/dto/IUpdateClientStateDTO'

@Component({
    selector: 'app-approve-reject-client',
    templateUrl: './approve-reject-client.component.html',
    styleUrls: ['./approve-reject-client.component.css'],
})
export class ApproveRejectClientComponent {
    clients: IClientWithoutPasswordDTO[]

    constructor(private service: ClientService) {
        this.clients = []
    }

    ngOnInit() {
        this.getPendingClients()
    }

    private getPendingClients() {
        this.service.getPendingClients().subscribe({
            next: (clients) => {
                this.clients = clients
            },
            error: (err) => {
                console.error(err)
                this.clients = []
            },
        })
    }

    rejectClient(email: string) {
        this.update({
            email: email,
            state: 'rejected',
        })
    }

    approveClient(email: string) {
        this.update({
            email: email,
            state: 'approved',
        })
    }

    private update(dto: IUpdateClientStateDTO) {
        this.service.updateClientState(dto).subscribe({
            next: (_client) => {
                // TODO: display message
                alert(
                    `Client with email ${
                        dto.email
                    } has been successfully ${dto.state.toLowerCase()}!`,
                )

                this.getPendingClients()
            },
            error: (err) => {
                console.error(JSON.stringify(err))
                alert('An error has occured!')
            },
        })
    }
}
