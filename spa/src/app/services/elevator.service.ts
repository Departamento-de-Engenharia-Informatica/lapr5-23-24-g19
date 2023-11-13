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

export interface ElevatorDTO{
    code:string
    name?:string
    description?:string
    maxFloorDimensions:{length:number,width:number}

}
@Injectable(
    // {
    // providedIn: AppModule
// }

)

export class ElevatorService{


    constructor(private http:HttpClient) {}

    getElevators(): Observable<ElevatorDTO[]> {
        const url = `${AppModule.baseUrl}/buildings`
        return this.http.get<ElevatorDTO[]>(url,{observe:'body',responseType:'json'})
    }
}
