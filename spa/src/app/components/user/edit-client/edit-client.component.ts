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
            name: [null, [Validators.required]],
            phoneNumber: [null, [Validators.required]],
            vatNumber: [null, [Validators.required]],
        })
    }

    ngOnInit(): void {
        // initialize email
    }

    getClient(): void {
        this.clientService
            .getClient(this.email)
            .subscribe((c: IClientWithoutPasswordDTO) => {
                this.client = c
            })
    }

    patchClient(): void {
        const dto: IClientWithoutPasswordDTO = {
            email: this.email,
            name: this.editClientForm.get('name')?.value,
            phoneNumber: this.editClientForm.get('phoneNumber')?.value,
            vatNumber: this.editClientForm.get('vatNumber')?.value,
        }

        this.clientService.patchClient(dto).subscribe((c: IClientWithoutPasswordDTO) => {
            this.client = c
        })
    }
}
