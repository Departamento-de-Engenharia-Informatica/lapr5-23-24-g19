import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, catchError, throwError } from 'rxjs'
import { AppModule } from '../app.module'
import {FloorPassageDTO} from "../dto/FloorPassageDTO";

export interface FloorAndBuildingDTO {
    buildingCode: string
    floorNumber: number
    description?: string
}

export interface PatchFloorDTO {
    buildingCode: string
    oldFloorNumber: number
    newFloorNumber?: number
    newDescription?: string
}

export interface PutFloorDTO {
    buildingCode: string
    oldFloorNumber: number
    newFloorNumber: number
    newDescription?: string
}

interface PatchDTO {
    floorNumber?: number
    description?: string
}

export interface UpdateMapDTO {
    map: string
}

interface FloorDTO {
    floorNumber: number
    description?: string
}

@Injectable({
    providedIn: 'root',
})
export class FloorService {
    constructor(private http: HttpClient) {}

    getFloors(buildingCode: string): Observable<FloorAndBuildingDTO[]> {
        return this.http.get<FloorAndBuildingDTO[]>(
            `${AppModule.baseUrl}/buildings/${buildingCode}/floors`,
            {
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    createFloor(dto: FloorAndBuildingDTO): Observable<FloorAndBuildingDTO> {
        const floor: FloorDTO = {
            floorNumber: dto.floorNumber,
            description: dto.description,
        }

        return this.http.post<FloorAndBuildingDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors`,
            JSON.stringify(floor),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    patchFloor(dto: PatchFloorDTO): Observable<FloorAndBuildingDTO> {
        const edit: PatchDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        }

        return this.http.patch<FloorAndBuildingDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
            JSON.stringify(edit),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    putFloor(dto: PutFloorDTO): Observable<FloorAndBuildingDTO> {
        const edit: FloorDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        }

        return this.http.put<FloorAndBuildingDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
            JSON.stringify(edit),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
    updateMap(
        dto: UpdateMapDTO,
        buildingCode: String,
        floorNumber: number,
    ): Observable<UpdateMapDTO> {
        return this.http.patch<UpdateMapDTO>(
            `${AppModule.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/map`,
            JSON.stringify(dto.map),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        ).pipe(catchError((response: HttpErrorResponse) => {
            let errorMessage: string;

            if (response.error) {
                errorMessage = response.error
            } else {
                errorMessage = `An unexpected error occurred: ${response.message}`;
            }

            return throwError(() => new Error(errorMessage))
        }))
    }

    getFloorsWithPassage(buildingCode: string): Observable<FloorPassageDTO[]> {
        return this.http.get<FloorPassageDTO[]>(
            `${AppModule.baseUrl}/buildings/${buildingCode}/floors/passages`,
            {
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
