import { Component, OnInit } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { ClientService } from 'src/app/services/client.service'
import { IClientWithoutPasswordDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'

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
    ) {
        this.editClientForm = this.formBuilder.group({
            name: [null],
            phoneNumber: [null],
            vatNumber: [null],
        })
    }

    ngOnInit(): void {
        this.email = 'joao.dias@isep.ipp.pt'
        this.getClient()
    }

    getClient(): void {
        this.clientService.getClient(this.email).subscribe(
            (c: IClientWithoutPasswordDTO) => {
                this.client = c
            },
            (error) => {
                alert('User not found!')

                this.email = ''
                this.editClientForm.reset()
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
                this.editClientForm.reset()
            },
            (error) => {
                alert(error.error)

                this.editClientForm.reset()
            },
        )
    }
}
