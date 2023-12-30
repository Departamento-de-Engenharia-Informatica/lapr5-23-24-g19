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
        const storedRoles = localStorage.getItem('USER_ROLES')?.toUpperCase()

        // Check if storedRoles string contains any of the roles from the roles array
        return roles.some((role) => storedRoles?.includes(RolesEnum[role]))
        // console.log("============================")
        // console.log(localStorage.getItem("USER_ROLES")?.toUpperCase())
        // console.log(JSON.stringify(roles[0]))
        // for(const role in roles){
        //     // console.log(this.rolesValues[role])
        //     if(localStorage.getItem("USER_ROLES")?.toUpperCase() === this.rolesValues[role])
        //         return true
        //     }
        // return localStorage.getItem("USER_ROLES")?.toUpperCase() === this.rolesValues[0]
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
