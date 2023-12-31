import { HttpClient, HttpHeaders } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, first, of, catchError, throwError, switchMap } from 'rxjs'
import { Config } from '../config'
import { ClientDTO } from '../dto/ClientDTO'
import { CreatedClientDTO } from '../dto/CreatedClientDTO'
import { IClientWithoutPasswordDTO } from '../../../../mdr/src/dto/IClientWithoutPasswordDTO'
import { ICreatedClientDTO } from '../../../../mdr/src/dto/ICreatedClientDTO'
import IUpdateClientStateDTO from '../../../../mdr/src/dto/IUpdateClientStateDTO'
import { IEditClientDTO } from '../dto/IEditClientDTO'
import { ClientEmailDTO } from '../dto/ClientEmailDTO'
import { AuthService } from '@auth0/auth0-angular'

@Injectable({
    providedIn: 'root',
})
export class ClientService {
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

    getClientData(dto: { email: string; password: string }) {
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.post<ArrayBuffer>(
                    `${Config.baseUrl}/clients/data`,
                    JSON.stringify(dto),
                    {
                        headers: this.authHeaders(token).set(
                            'Content-type',
                            'application/json',
                        ),
                        observe: 'body',
                        responseType: 'arraybuffer' as 'json',
                    },
                ),
            ),
            catchError((err) => throwError(() => err)),
        )
    }

    getClient(email: string): Observable<IClientWithoutPasswordDTO> {
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.get<ClientDTO>(`${Config.baseUrl}/clients/${email}`, {
                    headers: this.authHeaders(token),
                    observe: 'body',
                    responseType: 'json',
                }),
            ),
            catchError((err) => throwError(() => err)),
        )
    }

    getPendingClients(): Observable<ICreatedClientDTO[]> {
        // return this.http.get<ICreatedClientDTO[]>(
        //     `${Config.baseUrl}/clients?state=Pending`,
        // )
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.get<ICreatedClientDTO[]>(
                    `${Config.baseUrl}/clients?state=Pending`,
                    {
                        headers: this.authHeaders(token),
                        observe: 'body',
                        responseType: 'json',
                    },
                ),
            ),
            catchError((err) => throwError(() => err)),
        )
    }

    updateClientState(dto: IUpdateClientStateDTO): Observable<IClientWithoutPasswordDTO> {
        console.log(dto)
        // return this.http.patch<IClientWithoutPasswordDTO>(
        //     `${Config.baseUrl}/clients`,
        //     JSON.stringify(dto),
        //     {
        //         headers: { 'Content-type': 'application/json' },
        //         observe: 'body',
        //         responseType: 'json',
        //     },
        // )
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.patch<IClientWithoutPasswordDTO>(
                    `${Config.baseUrl}/clients`,
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
            catchError((err) => throwError(() => err)),
        )
    }

    patchClient(dto: IClientWithoutPasswordDTO): Observable<IClientWithoutPasswordDTO> {
        const editClientDTO: IEditClientDTO = {
            name: dto.name,
            phoneNumber: dto.phoneNumber,
            vatNumber: dto.vatNumber,
        }

        // return this.http.patch<IClientWithoutPasswordDTO>(
        //     `${Config.baseUrl}/clients/${dto.email}`,
        //     JSON.stringify(editClientDTO),
        //     {
        //         headers: { 'Content-type': 'application/json' },
        //         observe: 'body',
        //         responseType: 'json',
        //     },
        // )
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.patch<IClientWithoutPasswordDTO>(
                    `${Config.baseUrl}/clients/${dto.email}`,
                    JSON.stringify(editClientDTO),
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
            catchError((err) => throwError(() => err)),
        )
    }

    deleteClient(dto: ClientEmailDTO): Observable<ClientEmailDTO> {
        // return this.http.delete<ClientEmailDTO>(`${Config.baseUrl}/clients/${dto.email}`)
        return this.getToken().pipe(
            switchMap((token) =>
                this.http.delete<ClientEmailDTO>(
                    `${Config.baseUrl}/clients/${dto.email}`,
                    {
                        headers: this.authHeaders(token),
                        observe: 'body',
                        responseType: 'json',
                    },
                ),
            ),
            catchError((err) => throwError(() => err)),
        )
    }
}
