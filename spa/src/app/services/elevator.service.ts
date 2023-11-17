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

export interface CreatedElevatorDTO {
    buildingId: string
    identifier: number
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


    createElevator(dto: ElevatorDTO): Observable<CreatedElevatorDTO>{

        console.log("",dto.buildingId)
        console.log("",dto.floors)
        console.log("",dto.brand)
        console.log("",dto.model)
        console.log("",dto.serialNumber)
        console.log("",dto.description)



        const url = `${AppModule.baseUrl}/buildings/${dto.buildingId}/elevators`
        return this.http.post<CreatedElevatorDTO>(url,{observe:'body',responseType:'json'})
    }

    getElevators(buildingCode: string): Observable<CreatedElevatorDTO[]> {
        const url = `${AppModule.baseUrl}/buildings/${buildingCode}/elevators`
        return this.http.get<CreatedElevatorDTO[]>(url,{observe:'body',responseType:'json'})
    }
}
