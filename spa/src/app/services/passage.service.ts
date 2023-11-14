import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import {AppModule} from "../app.module";

export interface PassageDTO {
    floor1: {
        buildingCode: string
        floorNumber: number
    }

    floor2: {
        buildingCode: string
        floorNumber: number
    }
}


@Injectable({
    providedIn: 'root',
})
export class PassageService {
    constructor(private http: HttpClient) {}

    getPassagesBetweenBuildings(bCode1: string, bCode2: string): Observable<PassageDTO[]> {
        const url = `${AppModule.baseUrl}/passages/?building1=${bCode1}&building2=${bCode2}`
        return this.http.get<PassageDTO[]>(url,{observe:'body',responseType:'json'})
    }

}
