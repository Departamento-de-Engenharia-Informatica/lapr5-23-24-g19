import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

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
        const url = `http://localhost:4000/api/buildings/${buildingCode}/floors`;
        return this.http.get<FloorDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        });
    }

    createFloor(dto: FloorDTO): Observable<FloorDTO> {
        const url = `http://localhost:4000/api/buildings/${dto.buildingCode}/floors`;
        return this.http.post<FloorDTO>(url, {
            observe: 'body',
            responseType: 'json',
        });
    }
}
