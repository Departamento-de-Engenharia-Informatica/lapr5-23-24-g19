import {HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { Config } from '../config'
import {ClientDTO} from "../dto/ClientDTO";
import {CreatedClientDTO} from "../dto/CreatedClientDTO";
import {BackofficeUserDTO} from "../dto/BackofficeUserDTO";
import {CreatedBackofficeUserDTO} from "../dto/CreatedBackofficeUserDTO";

@Injectable({
    providedIn: 'root',
})
export class UserService {
    constructor(private http: HttpClient) {}

    createClient(dto: ClientDTO): Observable<CreatedClientDTO> {
        return this.http.post<CreatedClientDTO>(
            `${Config.baseUrl}/clients`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

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
