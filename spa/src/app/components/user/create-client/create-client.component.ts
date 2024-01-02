import { Component, EventEmitter, Output } from '@angular/core'
import {AbstractControl, FormBuilder, UntypedFormGroup, ValidationErrors, ValidatorFn, Validators} from '@angular/forms'

import { ClientDTO } from '../../../dto/ClientDTO'
import { ClientService } from '../../../services/client.service'

@Component({
    selector: 'app-create-client',
    templateUrl: './create-client.component.html',
    styleUrls: ['./create-client.component.css'],
})
export class CreateClientComponent {

    readonly pwdMinLength = 10
    form: UntypedFormGroup
    isPrivacyPolicyAgreed: boolean = false

    constructor(private fb: FormBuilder, private service: ClientService) {
        this.form = this.fb.group({
            name: [null, Validators.required],
            email: [null, [Validators.required, Validators.email]],
            vatNumber: [
                null,
                [Validators.required, Validators.minLength(9), Validators.maxLength(9)],
            ],
            phoneNumber: [
                null,
                [Validators.required, Validators.minLength(9), Validators.maxLength(9)],
            ],
            password: [
                null,
                [
                    Validators.required,
                    Validators.minLength(this.pwdMinLength),
                    Validators.pattern('^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[\\W_]).{10,}$') 
                ],

            ],
            confirmPassword: [null, [Validators.required, this.passwordMatchValidator()]],
        })

        this.form.get('password')?.valueChanges.subscribe(() => {
            this.form.get('confirmPassword')?.updateValueAndValidity()
        })
    }

    onPrivacyPolicyChange(event: any): void {
        this.isPrivacyPolicyAgreed = event.target.checked
    }
    submit() {
        if (this.form.valid && this.isPrivacyPolicyAgreed) {
            if (
                this.form.get('password')?.value !==
                this.form.get('confirmPassword')?.value
            ) {
                alert('Passwords do not match')
            } else {
                const dto: ClientDTO = {
                    name: this.form.value.name,
                    email: this.form.value.email,
                    phoneNumber: this.form.value.phoneNumber,
                    vatNumber: this.form.value.vatNumber,
                    password: this.form.value.password,
                }

                this.service.createClient(dto).subscribe({
                    next: (client) => {
                        alert(
                            `Created Client: \nName: ${client.name}\nEmail: ${client.email}\nPhoneNumber: ${client.phoneNumber}\nVatNumber: ${client.vatNumber}`,
                        )
                        this.form.reset()
                    },
                    error: (err) => {
                        alert('Error when creating user')
                        console.error(err)
                    },
                })
            }
        }
    }

    invalidField(field: string): boolean {
        return !!(
            this.form.get(field)?.invalid &&
            (this.form.get(field)?.dirty || this.form.get(field)?.touched)
        )
    }

    passwordMatchValidator(): ValidatorFn {
        return (control: AbstractControl): ValidationErrors | null => {
            const password = control.get('password')
            const confirmPassword = control.get('confirmPassword')

            if (!password || !confirmPassword) {
                return null
            }

            return password.value === confirmPassword.value ? null : { mismatch: true }
        }
    }
}
