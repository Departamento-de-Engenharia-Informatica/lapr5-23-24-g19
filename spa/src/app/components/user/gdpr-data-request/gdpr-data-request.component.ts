import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { AuthService, User } from '@auth0/auth0-angular'
import { ClientService } from 'src/app/services/client.service'

import { switchMap, take, throwError } from 'rxjs'
import { MatDialogRef } from '@angular/material/dialog'

@Component({
    selector: 'app-gdpr-data-request',
    templateUrl: './gdpr-data-request.component.html',
    styleUrls: ['./gdpr-data-request.component.css'],
})
export class GdprDataRequestComponent {
    form: FormGroup

    constructor(
        private dialogRef: MatDialogRef<GdprDataRequestComponent>,
        private service: ClientService,
        private auth: AuthService,
        private fb: FormBuilder,
    ) {
        this.form = this.fb.group({
            password: ['', Validators.required],
        })
    }

    ngOnInit() {}

    onNoClick(): void {
        this.dialogRef.close()
    }

    submit() {
        this.auth.user$
            .pipe(
                take(1),
                switchMap((user) => this.getClientData(user)),
            )
            .subscribe({
                next: (data) => {
                    this.form.reset()
                    this.dialogRef.close()
                    this.downloadFile(data)
                },
                error: (err) => {
                    if (err.status === 403) {
                        alert('Bad Password')
                    } else {
                        alert(err.statusText)
                    }
                    console.error(JSON.stringify(err, null, 2))
                },
            })
    }

    private getClientData(user?: User | null) {
        if (user) {
            const requestData = {
                email: user.email!,
                password: this.form.get('password')?.value,
            }
            return this.service.getClientData(requestData)
        } else {
            return throwError(() => new Error('User is null'))
        }
    }

    private downloadFile(data: ArrayBuffer): void {
        const blob = new Blob([data], { type: 'application/zip' })
        const url = URL.createObjectURL(blob)

        const a = document.createElement('a')
        a.href = url

        const timestamp = new Date().toISOString().replace(/[:\/\\*\?"<>\|]/g, '_')
        a.download = `userdata_${timestamp}.zip`
        a.target = '_blank'

        document.body.appendChild(a)
        a.click()

        window.URL.revokeObjectURL(url)
        document.body.removeChild(a)
    }
}
