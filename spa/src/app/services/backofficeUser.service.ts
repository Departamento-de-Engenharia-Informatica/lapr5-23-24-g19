import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { Config } from '../config'
import { BackofficeUserDTO } from '../dto/BackofficeUserDTO'
import { CreatedBackofficeUserDTO } from '../dto/CreatedBackofficeUserDTO'

@Injectable({
    providedIn: 'root',
})
export class BackofficeUserService {
    constructor(private http: HttpClient) {}

    createBackofficeUser(dto: BackofficeUserDTO): Observable<CreatedBackofficeUserDTO> {
        return this.http.post<CreatedBackofficeUserDTO>(
            `${Config.baseUrl}/users-backoffice`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
