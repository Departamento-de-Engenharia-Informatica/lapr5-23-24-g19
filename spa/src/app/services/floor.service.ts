import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

export interface Floor {
    buildingCode: string;
    floorNumber: number;
    description?: string;
}

export interface Building {
    code: string;
    name: string;
}

@Injectable({
    providedIn: 'root',
})
export class FloorService {
    constructor(private http: HttpClient) {}

    // TODO: this should be handled by BuildingService
    getBuildings(): Observable<Building[]> {
        const url = `http://localhost:4000/api/buildings/`;
        return this.http.get<Building[]>(url, {
            observe: 'body',
            responseType: 'json',
        });
    }

    getFloors(buildingCode: string): Observable<Floor[]> {
        const url = `http://localhost:4000/api/buildings/${buildingCode}/floors`;
        return this.http.get<Floor[]>(url, {
            observe: 'body',
            responseType: 'json',
        });
    }
}
