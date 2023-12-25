import { Component, OnInit } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { ClientService } from 'src/app/services/client.service'
import { IClientWithoutPasswordDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'
import { AuthService } from '@auth0/auth0-angular'

@Component({
    selector: 'app-edit-client',
    templateUrl: './edit-client.component.html',
    styleUrls: ['./edit-client.component.css'],
})
export class EditClientComponent implements OnInit {
    email: string = ''
    client!: IClientWithoutPasswordDTO

    editClientForm: FormGroup

    constructor(
        private formBuilder: FormBuilder,
        private clientService: ClientService,
        public auth: AuthService,
    ) {
        this.editClientForm = this.formBuilder.group({
            name: [''],
            phoneNumber: [''],
            vatNumber: [''],
        })
    }

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
            (error) => {
                this.email = ''
                this.editClientForm.reset({
                    name: '',
                    phoneNumber: '',
                    vatNumber: '',
                })
            },
        )
    }

    emailExists(): boolean {
        return this.email.length !== 0
    }

    onSubmit(): void {
        const dto: IClientWithoutPasswordDTO = {
            email: this.email,
        }

        const name = this.editClientForm.value.name
        if (name !== null && name !== '') dto.name = name

        const phoneNumber = this.editClientForm.value.phoneNumber
        if (phoneNumber !== null && phoneNumber !== '') dto.phoneNumber = phoneNumber

        const vatNumber = this.editClientForm.value.vatNumber
        if (vatNumber !== null && vatNumber !== '') dto.vatNumber = vatNumber

        this.clientService.patchClient(dto).subscribe(
            (c: IClientWithoutPasswordDTO) => {
                let alertMessage = `Client ${this.email} edited successfully!`

                alertMessage += `\nName: ${c.name}`
                alertMessage += `\nPhone number: ${c.phoneNumber}`
                alertMessage += `\nVAT number: ${c.vatNumber}`

                alert(alertMessage)

                this.client = c
                this.editClientForm.reset({
                    name: '',
                    phoneNumber: '',
                    vatNumber: '',
                })
            },
            (error) => {
                alert(error.error)

                this.editClientForm.reset({
                    name: '',
                    phoneNumber: '',
                    vatNumber: '',
                })
            },
        )
    }
}
