import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'

import { ClientService } from '../../../services/client.service'
import {ClientEmailPasswordDTO} from "../../../dto/ClientEmailPasswordDTO";
import {AuthService} from "@auth0/auth0-angular";

@Component({
    selector: 'app-delete-client',
    templateUrl: './delete-client.component.html',
    styleUrls: ['./delete-client.component.css'],
})
export class DeleteClientComponent {
    /*form: UntypedFormGroup

    constructor(
        private fb: FormBuilder,
        private service: ClientService,
        public auth: AuthService

    ) {
        this.form = this.fb.group({
            email: [null, Validators.required],
            password: [null, Validators.required],
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
    submit() {
        if (this.form.valid) {
            const dto: ClientEmailPasswordDTO = {
                email: this.form.value.email,
                password: this.form.value.password,
            }

            this.service.deleteClient(dto).subscribe({
                next: (client) => {
                    alert(
                        `Deleted Account`,
                    )
                    this.form.reset()
                },
                error: (error) => alert(JSON.stringify(error.error)),
            })
        }
    }*/
}
