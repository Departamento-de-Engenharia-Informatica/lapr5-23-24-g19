import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { AppModule } from '../app.module';

export interface FloorDTO {
    buildingCode: string;
    floorNumber: number;
    description?: string;
}

export interface ListFloorsDTO {
    buildingCode: string;
}

@Injectable({
    providedIn: 'root',
})
export class FloorService {
    constructor(private http: HttpClient) {}

    getFloors(buildingCode: string): Observable<FloorDTO[]> {
        return this.http.get<FloorDTO[]>(
            `${AppModule.baseUrl}/buildings/${buildingCode}/floors`,
            {
                observe: 'body',
                responseType: 'json',
            },
        );
    }

    createFloor(dto: FloorDTO): Observable<FloorDTO> {
        return this.http.post<FloorDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        );
    }
}
