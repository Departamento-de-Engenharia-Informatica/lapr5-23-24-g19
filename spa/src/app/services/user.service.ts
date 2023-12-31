import { Injectable } from '@angular/core'
import { Observable, catchError, first, firstValueFrom, throwError } from 'rxjs'
import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http'
import { AuthService } from '@auth0/auth0-angular'
import { forEach } from 'cypress/types/lodash'

@Injectable({
    providedIn: 'root',
})
export class UserService {
    private rolesValues = Object.values(RolesEnum)

    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    private getTokenObservable(): Observable<string> {
        return this.auth.getAccessTokenSilently().pipe(
            first(),
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

    hasRole(roles: RolesEnum[]): boolean {
        // Retrieve the roles from localStorage and split them into an array
        const storedRolesString = localStorage.getItem('USER_ROLES')
        const storedRoles = storedRolesString ? storedRolesString.split(',') : []

        // Normalize roles for case-insensitive comparison
        const normalizedStoredRoles = storedRoles.map((role) => role.toUpperCase())

        // Check if any of the provided roles match the stored roles
        return roles.some((role) =>
            normalizedStoredRoles.includes(RolesEnum[role].toUpperCase()),
        )
    }
}

export enum RolesEnum {
    ADM = 'ADM',
    CLT = 'CLT',
    CMP = 'CMP',
    FLM = 'FLM',
    SYSADM = 'SYSADM',
    TKM = 'TKM',
}
