import { Component, Inject, OnInit } from '@angular/core'
import { AuthService } from '@auth0/auth0-angular'
import { DOCUMENT } from '@angular/common'

@Component({
    selector: 'app-auth',
    templateUrl: './auth.component.html',
    styleUrls: ['./auth.component.css'],
})
export class AuthComponent implements OnInit {
    constructor(@Inject(DOCUMENT) public document: Document, public auth: AuthService) {
        auth.loginWithRedirect()
    }
    ngOnInit(): void {}

    // this.s = JSON.stringify(auth)
}
