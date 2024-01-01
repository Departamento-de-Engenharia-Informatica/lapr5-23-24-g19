import { HttpClient, HttpHeaders } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, first, throwError, catchError, switchMap } from 'rxjs'
import { Config } from '../config'
import { BackofficeUserDTO } from '../dto/BackofficeUserDTO'
import { CreatedBackofficeUserDTO } from '../dto/CreatedBackofficeUserDTO'
import { AuthService } from '@auth0/auth0-angular'

export type RoleDTO = { name: string }

@Injectable({
    providedIn: 'root',
})
export class BackofficeUserService {
    constructor(private http: HttpClient, private auth: AuthService) {}

    private getToken(): Observable<string> {
        return this.auth.getAccessTokenSilently().pipe(
            first(), // Take the first emitted value and complete the observable
            catchError((error) => {
                console.error('Error getting token:', error)
                return throwError(() => new Error('Unable to get authentication token.'))
            }),
        )
    }

    private authHeaders(token: string): HttpHeaders {
        return new HttpHeaders({
            Authorization: `Bearer ${token}`,
        })
    }

    getRoles() {
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.get<RoleDTO[]>(`${Config.baseUrl}/roles`, {
                    headers: this.authHeaders(token),
                    observe: 'body',
                    responseType: 'json',
                }),
            ),
        )
    }

    createBackofficeUser(dto: BackofficeUserDTO): Observable<CreatedBackofficeUserDTO> {
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.post<CreatedBackofficeUserDTO>(
                    `${Config.baseUrl}/users-backoffice`,
                    JSON.stringify(dto),
                    {
                        headers: this.authHeaders(token).set(
                            'Content-type',
                            'application/json',
                        ),
                        observe: 'body',
                        responseType: 'json',
                    },
                ),
            ),
        )
    }
}
