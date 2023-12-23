import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { Config } from '../config'
import { ClientDTO } from '../dto/ClientDTO'
import { CreatedClientDTO } from '../dto/CreatedClientDTO'
import { IClientWithoutPasswordDTO } from '../../../../mdr/src/dto/IClientWithoutPasswordDTO'
import { IEditClientDTO } from '../dto/IEditClientDTO'

@Injectable({
    providedIn: 'root',
})
export class ClientService {
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

    getClient(email: string): Observable<IClientWithoutPasswordDTO> {
        return this.http.get<ClientDTO>(`${Config.baseUrl}/clients/${email}`)
    }

    patchClient(dto: IClientWithoutPasswordDTO): Observable<IClientWithoutPasswordDTO> {
        const editClientDTO: IEditClientDTO = {
            name: dto.name,
            phoneNumber: dto.phoneNumber,
            vatNumber: dto.vatNumber,
        }

        return this.http.patch<IClientWithoutPasswordDTO>(
            `${Config.baseUrl}/clients/${dto.email}`,
            JSON.stringify(editClientDTO),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}