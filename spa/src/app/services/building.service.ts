import { HttpClient, HttpParams, HttpResponse } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AppModule } from '../app.module';
import { Observable } from 'rxjs';

// interface BuildingProps{
//   code:string
// }

// @Injectable({
//   providedIn: 'root'
// })

export interface BuildingDTO {
    code: string;
    name: string;
    description: string;
    maxFloorDimensions: { length: number; width: number };
}
export interface EditBuildingDTO {
    name?: string;
    description?: string;
    maxFloorDimensions?: { length: number; width: number };
}

export interface BuildingByFloorsDTO {
    code: string;
    name?: string;
    description?: string;
    maxFloorDimensions: { length: number; width: number };
    floorNumber: number;
}

export interface MinMaxDTO {
    min: number;
    max: number;
}

@Injectable()
// {
// providedIn: AppModule
// }
export class BuildingService {
    constructor(private http: HttpClient) { }

    getBuildings(): Observable<BuildingDTO[]> {
        const url = `${AppModule.baseUrl}/buildings`;
        return this.http.get<BuildingDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        });
    }

    getBuildingsByFloors(dto: MinMaxDTO): Observable<BuildingByFloorsDTO[]> {
        let params = new HttpParams();
        params = params.set('minFloors', dto.min);
        params = params.set('maxFloors', dto.max);

        return this.http.get<BuildingByFloorsDTO[]>(
            `${AppModule.baseUrl}/buildings/`,
            {
                params,
                observe: 'body',
                responseType: 'json',
            },
        );
    }

    createBuilding(building: BuildingDTO) {
        this.http.post(
            `${AppModule.baseUrl}/building`,
            JSON.stringify(building),
            {
                headers: { 'Content-type': 'application/json' },
            },
        );
    }
    putBuilding(building: EditBuildingDTO, buildingCode: String): Observable<BuildingDTO> {
        return this.http.put<BuildingDTO>(
            `${AppModule.baseUrl}/buildings/${buildingCode}`,
            JSON.stringify(cleanObject(building)),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
    patchBuilding(building: EditBuildingDTO, buildingCode: String): Observable<BuildingDTO> {
        return this.http.patch<BuildingDTO>(
            `${AppModule.baseUrl}/buildings/${buildingCode}`,
            JSON.stringify(cleanObject(building)),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
function cleanObject(obj: any) {
    const cleanedObj = {} as any;

    for (const key in obj) {
        if (obj[key] !== null && obj[key] !== undefined && obj[key] != '') {
            cleanedObj[key] = obj[key];
        }
    }

    return cleanedObj;
}
