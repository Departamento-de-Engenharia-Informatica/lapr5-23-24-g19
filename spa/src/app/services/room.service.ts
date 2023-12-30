import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, firstValueFrom } from 'rxjs'
import { CreatedRoomDTO } from 'src/app/dto/CreatedRoomDTO'
import { RoomDTO } from 'src/app/dto/RoomDTO'
import { Config } from '../config'
import { AuthService } from '@auth0/auth0-angular'

@Injectable({
    providedIn: 'root',
})
export class RoomService {
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    createRoom(
        buildingCode: string,
        floorNumber: string,
        dto: RoomDTO,
    ): Observable<CreatedRoomDTO> {
        return new Observable<CreatedRoomDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<CreatedRoomDTO>(
                            `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
                            JSON.stringify(dto),
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                    'Content-type': 'application/json',
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (createdRoom) => {
                                observer.next(createdRoom)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }

    getRooms(
        buildingCode: string,
        floorNumber: string | number,
    ): Observable<CreatedRoomDTO[]> {
        return new Observable<CreatedRoomDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<CreatedRoomDTO[]>(
                            `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (rooms) => {
                                observer.next(rooms)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }
}
