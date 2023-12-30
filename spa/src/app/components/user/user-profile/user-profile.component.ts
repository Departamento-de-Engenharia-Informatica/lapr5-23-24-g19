import { Component } from '@angular/core'
import { MatDialog } from '@angular/material/dialog'
import { GdprDataRequestComponent } from '../gdpr-data-request/gdpr-data-request.component'
import { ClientService } from 'src/app/services/client.service'
import { AuthService } from '@auth0/auth0-angular'
import { switchMap } from 'rxjs'
import { IClientWithoutPasswordDTO as ClientDTO } from '../../../../../../mdr/src/dto/IClientWithoutPasswordDTO'

@Component({
    selector: 'app-user-profile',
    templateUrl: './user-profile.component.html',
    styleUrls: ['./user-profile.component.css'],
})
export class UserProfileComponent {
    client?: ClientDTO

    constructor(
        private auth: AuthService,
        private service: ClientService,
        private dialog: MatDialog,
    ) {}

    ngOnInit() {
        this.auth.user$
            .pipe(switchMap((user) => this.service.getClient(user?.email!)))
            .subscribe({
                next: (data) => (this.client = data),
                error: (err) => {
                    console.error(err)
                    alert('Failed to retrieve client information')
                },
            })
    }

    openPasswordDialog(): void {
        this.dialog.open(GdprDataRequestComponent, {
            width: '300px',
            autoFocus: false,
        })
    }
}
