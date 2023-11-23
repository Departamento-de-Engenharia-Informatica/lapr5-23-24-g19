import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import {catchError, Observable, throwError} from 'rxjs'
import { AppModule } from '../app.module'

export interface RoomDTO {
    name: string
    description: string
    category: string
    dimensions: { length: number; width: number }
    positions: { x: number; y: number }
}

export interface CreatedRoomDTO {
    name: string
    buildingCode: string
    floorNumber: number
    description: string
    category: string
    dimensions: { length: number; width: number }
    positions: { x: number; y: number }
}

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
            `${AppModule.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        ).pipe(
            catchError((error: any) => {
                if (error.status === 422) {
                    const errorMessage = error.error.message || 'Bad Request'
                    console.log(`Error: ${errorMessage}`)
                    return throwError(errorMessage)
                } else {
                    console.log(`Error: ${error.message}`)
                    return throwError(
                        'An unexpected error occurred. Please try again later.',
                    )
                }
            }),
        )
    }

    getRooms(buildingCode: string, floorNumber: string): Observable<CreatedRoomDTO[]> {
        const url = `${AppModule.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`
        return this.http.get<CreatedRoomDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        })
    }
}
