import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'

import { BackofficeUserDTO } from '../../../dto/BackofficeUserDTO'
import { BackofficeUserService } from '../../../services/backofficeUser.service'


enum RoleType {
    CAMPUS_MANAGER = 'Campus Manager',
    FLEET_MANAGER = 'Fleet Manager',
    TASK_MANAGER = 'Task Manager',
    ADMINISTRATOR = 'Administrator',
    SYSTEMS_ADMINISTRATOR = 'Systems Administrator'
}
@Component({
    selector: 'app-create-backoffice-user',
    templateUrl: './create-backoffice-user.component.html',
    styleUrls: ['./create-backoffice-user.component.css'],
})
export class CreateBackofficeUserComponent {

    roleTypes = Object.values(RoleType)
    form: UntypedFormGroup

    constructor(private fb: FormBuilder, private service: BackofficeUserService) {
        this.form = this.fb.group({
            name: [null, Validators.required],
            role: [null, Validators.required],
            email: [null, Validators.required],
            phoneNumber: [null, Validators.required],
            password: [null, Validators.required],
        })
    }
    submit() {
        if (this.form.valid) {
            const dto: BackofficeUserDTO = {
                name: this.form.value.name,
                role: this.form.value.role,
                email: this.form.value.email,
                phoneNumber: this.form.value.phoneNumber,
                password: this.form.value.password,
            }

            this.service.createBackofficeUser(dto).subscribe({
                next: (backofficeUser) => {
                    alert(
                        `Created Backoffice User: \nName: ${backofficeUser.name}\nRole: ${backofficeUser.role}\nEmail: ${backofficeUser.email}\nPhoneNumber: ${backofficeUser.phoneNumber}`,
                    )
                    this.form.reset()
                },
                error: (error) => alert(JSON.stringify(error.error)),
            })
        }
    }
}
