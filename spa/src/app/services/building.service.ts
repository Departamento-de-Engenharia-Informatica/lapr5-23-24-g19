import { HttpClient, HttpResponse } from '@angular/common/http';
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
    name?: string;
    description?: string;
    maxFloorDimensions: { length: number; width: number };
}

@Injectable()
// {
// providedIn: AppModule
// }
export class BuildingService {
    constructor(private http: HttpClient) {}

    getBuildings(): Observable<BuildingDTO[]> {
        const url = `${AppModule.baseUrl}/buildings`;
        return this.http.get<BuildingDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        });
    }
}
