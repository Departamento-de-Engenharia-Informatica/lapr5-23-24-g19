import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { AppModule } from '../app.module';

export interface FloorAndBuildingDTO {
    buildingCode: string;
    floorNumber: number;
    description?: string;
}

export interface PatchFloorDTO {
    buildingCode: string;
    oldFloorNumber: number;
    newFloorNumber?: number;
    newDescription?: string;
}

export interface PutFloorDTO {
    buildingCode: string;
    oldFloorNumber: number;
    newFloorNumber: number;
    newDescription?: string;
}

interface PatchDTO {
    floorNumber?: number;
    description?: string;
}

interface FloorDTO {
    floorNumber: number;
    description?: string;
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
        );
    }

    createFloor(dto: FloorAndBuildingDTO): Observable<FloorAndBuildingDTO> {
        const floor: FloorDTO = {
            floorNumber: dto.floorNumber,
            description: dto.description,
        };

        return this.http.post<FloorAndBuildingDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors`,
            JSON.stringify(floor),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        );
    }

    patchFloor(dto: PatchFloorDTO): Observable<FloorAndBuildingDTO> {
        const edit: PatchDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        };

        return this.http.patch<FloorAndBuildingDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
            JSON.stringify(edit),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        );
    }

    putFloor(dto: PutFloorDTO): Observable<FloorAndBuildingDTO> {
        const edit: FloorDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        };

        return this.http.put<FloorAndBuildingDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
            JSON.stringify(edit),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        );
    }
}
