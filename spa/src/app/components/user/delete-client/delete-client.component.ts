import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, FormGroup, UntypedFormGroup, Validators } from '@angular/forms'

import { ClientService } from '../../../services/client.service'

import { IClientWithoutPasswordDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'
import { AuthService } from '@auth0/auth0-angular'
import { ClientEmailDTO } from '../../../dto/ClientEmailDTO'
import { MatDialogRef } from '@angular/material/dialog'
import { take, switchMap } from 'rxjs'

@Component({
    selector: 'app-delete-client',
    templateUrl: './delete-client.component.html',
    styleUrls: ['./delete-client.component.css'],
})
export class DeleteClientComponent {
    // client!: IClientWithoutPasswordDTO
    form: FormGroup

    constructor(
        private dialogRef: MatDialogRef<DeleteClientComponent>,
        private clientService: ClientService,
        private fb: FormBuilder,
        public auth: AuthService
    ) {
        this.form = this.fb.group({
            password: ['', Validators.required],
        })
    }

    ngOnInit(): void {
        // this.auth.isAuthenticated$.subscribe((isAuthenticated) => {
        //     if (isAuthenticated) {
        //         console.log('Authenticated')
        //         this.auth.user$.subscribe((user) => {
        //             this.email = user?.email as string
        //             this.getClient()
        //         })
        //     }
        // })
    }

    onNoClick() {
        this.dialogRef.close()
    }

    // getClient(): void {
    //     this.auth.user$.pipe(
    //         take(1),
    //         switchMap(user =>
    //                   this.clientService.getClient(user!.email!)
    //         )
    //     ).subscribe({
    //         next: (c) => {
    //             this.dialogRef.close()
    //             this.client = c
    //         }
    //     })

    //     this.clientService.getClient(this.email).subscribe(
    //         (c: IClientWithoutPasswordDTO) => {
    //             this.client = c
    //         },
    //         (error) => alert(JSON.stringify(error.error)),
    //     )
    // }

    onSubmit() {
        this.auth.user$.pipe(
            take(1),
            switchMap(user =>
                this.clientService.deleteClient({ email: user!.email! })
            )
        ).subscribe({


        // this.clientService.deleteClient(dto).subscribe({
            next: (client) => {
                alert(`Deleted Account \nEmail: ${client.email}`)
                this.form.reset()
                this.dialogRef.close()
                this.auth.logout()
            },
            error: (error) => alert(JSON.stringify(error.error)),
        })
    }
}
