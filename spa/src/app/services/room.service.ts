import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { CreatedRoomDTO } from 'src/app/dto/CreatedRoomDTO'
import { RoomDTO } from 'src/app/dto/RoomDTO'
import { Config } from '../config'

@Injectable({
    providedIn: 'root',
})
export class RoomService {
    constructor(private http: HttpClient) {}

    createRoom(
        buildingCode: string,
        floorNumber: string,
        dto: RoomDTO,
    ): Observable<CreatedRoomDTO> {
        return this.http.post<CreatedRoomDTO>(
            `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    getRooms(
        buildingCode: string,
        floorNumber: string | number,
    ): Observable<CreatedRoomDTO[]> {
        const url = `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`
        return this.http.get<CreatedRoomDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        })
    }
}
