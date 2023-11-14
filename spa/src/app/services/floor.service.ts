import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

export interface Floor {
    buildingCode: string;
    floorNumber: number;
    description?: string;
}

@Injectable({
    providedIn: 'root',
})
export class FloorService {
    constructor(private http: HttpClient) {}

    getFloors(buildingCode: string): Observable<Floor[]> {
        const url = `http://localhost:4000/api/buildings/${buildingCode}/floors`;
        return this.http.get<Floor[]>(url, {
            observe: 'body',
            responseType: 'json',
        });
    }
}
