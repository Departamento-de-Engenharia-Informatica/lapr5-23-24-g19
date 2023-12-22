import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'

import {UserService} from "../../../services/user.Service";
import {BackofficeUserDTO} from "../../../dto/BackofficeUserDTO";

@Component({
    selector: 'app-create-backoffice-user',
    templateUrl: './create-backoffice-user.component.html',
    styleUrls: ['./create-backoffice-user.component.css'],
})
export class CreateBackofficeUserComponent {
    form: UntypedFormGroup


    constructor(
        private fb: FormBuilder,
        private service: UserService,
    ) {
        this.form = this.fb.group({
            name: [null, Validators.required],
            email: [null, Validators.required],
            phoneNumber: [null, Validators.required],
            password: [null, Validators.required],
        })
    }
    submit() {
        if (this.form.valid) {
            const dto: BackofficeUserDTO = {
                name: this.form.value.name,
                email: this.form.value.email,
                phoneNumber: this.form.value.phoneNumber,
                password: this.form.value.password,
            }

            this.service.createBackofficeUser(dto).subscribe({
                next: (backofficeUser) => {
                    alert(
                        `Created Backoffice User: \nName: ${backofficeUser.name}\nEmail: ${backofficeUser.email}\nPhoneNumber: ${backofficeUser.phoneNumber}`
                    )
                    this.form.reset()
                },
                error: (error) => alert(JSON.stringify(error)),
            })
        }
    }
}
