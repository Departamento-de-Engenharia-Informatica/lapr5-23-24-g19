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
    buildingId: string
    floors: number[]

    brand?: string
    model?: string
    serialNumber?: string
    description?: string

}
@Injectable(
    // {
    // providedIn: AppModule
// }

)

export class ElevatorService{


    constructor(private http:HttpClient) {}


    createElevator(elevator: ElevatorDTO): Observable<ElevatorDTO>{
        const url = `${AppModule.baseUrl}/buildings/:id/elevators`
        return this.http.post<ElevatorDTO>(url,{observe:'body',responseType:'json'})
    }

    getElevators(): Observable<ElevatorDTO[]> {
        const url = `${AppModule.baseUrl}/buildings/:id/elevators`
        return this.http.get<ElevatorDTO[]>(url,{observe:'body',responseType:'json'})
    }
}
