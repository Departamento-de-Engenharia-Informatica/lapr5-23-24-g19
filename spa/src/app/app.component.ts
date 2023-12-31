import { Component, isDevMode } from '@angular/core'
import {
    EMPTY,
    Observable,
    Subscription,
    catchError,
    delay,
    firstValueFrom,
    interval,
    retry,
    retryWhen,
    switchMap,
} from 'rxjs'
import { AppModule } from './app.module'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { AuthService, User } from '@auth0/auth0-angular'
import { UserDTO } from './dto/UserDTO'
import { Config } from './config'

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html',
    styleUrls: ['./app.component.css'],
})
export class AppComponent {
    title!: String
    isBackendUp!: boolean

    constructor(public auth: AuthService, private http: HttpClient) {
        this.auth.isAuthenticated$.subscribe((isAuthenticated) => {
            if (isAuthenticated) {
                this.auth.getAccessTokenSilently().subscribe((token) => {
                    for (var i = 0; i < 10; i++) {
                        AppModule.authToken = token
                        console.log('Bearer ', token)
                    }
                    this.getMe().subscribe()
                })
            }
        })
    }

    checkBackendStatus(): void {
        interval(3000)
            .pipe(
                switchMap(() =>
                    this.http.get(`${AppModule.mdrUrl}/status`).pipe(
                        catchError(() => {
                            console.log('Backend is down')
                            this.isBackendUp = false
                            return EMPTY
                        }),
                    ),
                ),
                retry(), // Retry the entire observable sequence on error
                delay(3000), // Delay between retries
            )
            .subscribe(() => {
                this.isBackendUp = true
            })
    }

    ngOnInit(): void {
        this.title = 'RobDroneGo'
        this.isBackendUp = false
        this.checkBackendStatus()
        // TODO: ATIVAR LOGIN
        // this.auth.isAuthenticated$.subscribe((loggedIn: boolean) => {
        //   if (!loggedIn) {
        //     // Redirect to Auth0 login
        //     this.auth.loginWithRedirect();
        //   }
        // });
    }

    private getMe(): Observable<string> {
        const url = `${Config.baseUrl}/auth/me`

        return new Observable<string>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<string>(url, {
                            headers: { Authorization: `Bearer ${token}` },
                        })
                        .subscribe(
                            (user) => {
                                // console.log("sub")
                                // console.log(JSON.stringify(user))
                                const u = user as unknown as UserDTO
                                localStorage.setItem('USER_ROLES', u.roles.toString())
                                console.log('Has roles: ', u.roles.toString())
                                // alert(localStorage.getItem("USER_ROLES"))
                            },
                            (error) => {
                                const errorMessage =
                                    error instanceof HttpErrorResponse
                                        ? error.error ||
                                          `An unexpected error occurred: ${error.message}`
                                        : 'An unexpected error occurred'
                                observer.error(new Error(errorMessage))
                            },
                            () => observer.complete(),
                        )
                })
                .catch((error) => observer.error(error))
        })
    }

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }
    logout() {
        this.auth.logout()
        localStorage.removeItem('USER_ROLES')
    }
}
