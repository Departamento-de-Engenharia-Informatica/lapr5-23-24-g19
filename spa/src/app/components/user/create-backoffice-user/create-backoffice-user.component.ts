import { Component } from '@angular/core'
import {
    AbstractControl,
    FormBuilder,
    UntypedFormGroup,
    ValidationErrors,
    ValidatorFn,
    Validators,
} from '@angular/forms'

import { BackofficeUserDTO } from '../../../dto/BackofficeUserDTO'
import { BackofficeUserService, RoleDTO } from '../../../services/backofficeUser.service'

@Component({
    selector: 'app-create-backoffice-user',
    templateUrl: './create-backoffice-user.component.html',
    styleUrls: ['./create-backoffice-user.component.css'],
})
export class CreateBackofficeUserComponent {
    readonly pwdMinLength = 10
    form: UntypedFormGroup

    roles: RoleDTO[] = []

    constructor(private fb: FormBuilder, private service: BackofficeUserService) {
        this.form = this.fb.group({
            name: [null, Validators.required],
            role: [null, Validators.required],
            email: [null, [Validators.required, Validators.email]],
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

    ngOnInit() {
        this.service.getRoles().subscribe({
            next: (roles) => {
                const customSort = (a: RoleDTO, b: RoleDTO) => {
                    const trimmedA = a.name.replace(/\s/g, '')
                    const trimmedB = b.name.replace(/\s/g, '')

                    return trimmedA.localeCompare(trimmedB)
                }
                this.roles = roles.sort(customSort)
            },
            error: (err) => {
                console.log(err)
                alert('Failed to fetch Roles')
            },
        })
    }

    submit() {
        if (this.form.valid) {
            if (
                this.form.get('password')?.value !==
                this.form.get('confirmPassword')?.value
            ) {
                alert('Passwords do not match')
            } else {
                const dto: BackofficeUserDTO = {
                    name: this.form.value.name,
                    role: this.form.value.role,
                    email: this.form.value.email,
                    phoneNumber: this.form.value.phoneNumber,
                    password: this.form.value.password,
                }

                this.service.createBackofficeUser(dto).subscribe({
                    next: (_user) => {
                        alert('Created user with success')
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
