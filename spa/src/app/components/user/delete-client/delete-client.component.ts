import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'

import { ClientService } from '../../../services/client.service'

import { IClientWithoutPasswordDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'
import { AuthService } from '@auth0/auth0-angular'
import { ClientEmailDTO } from '../../../dto/ClientEmailDTO'

@Component({
    selector: 'app-delete-client',
    templateUrl: './delete-client.component.html',
    styleUrls: ['./delete-client.component.css'],
})
export class DeleteClientComponent {
    email: string = ''
    client!: IClientWithoutPasswordDTO

    constructor(private clientService: ClientService, public auth: AuthService) {}

    ngOnInit(): void {
        this.auth.isAuthenticated$.subscribe((isAuthenticated) => {
            if (isAuthenticated) {
                console.log('Authenticated')
                this.auth.user$.subscribe((user) => {
                    this.email = user?.email as string
                    this.getClient()
                })
            }
        })
    }

    getClient(): void {
        this.clientService.getClient(this.email).subscribe(
            (c: IClientWithoutPasswordDTO) => {
                this.client = c
            },
            (error) => alert(JSON.stringify(error.error)),
        )
    }
    onSubmit() {
        const dto: ClientEmailDTO = {
            email: this.email,
        }

        this.clientService.deleteClient(dto).subscribe({
            next: (client) => {
                alert(`Deleted Account \nEmail: ${client.email}`)
                this.auth.logout()
            },
            error: (error) => alert(JSON.stringify(error.error)),
        })
    }
}
