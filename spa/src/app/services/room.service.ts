import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { AppModule } from '../app.module';

export interface RoomDTO {
    name: string
    buildingCode: string
    floorNumber: number
    description: string
    category: string
    dimensions: { length: number; width: number }
    positions: { x: number; y: number }
}

export interface FloorAndBuildingDTO {
    buildingCode: string;
    floorNumber: number;
    description?: string;
}

@Injectable({
    providedIn: 'root',
})
export class RoomService {
    constructor(private http: HttpClient) {}



    createRoom(dto: RoomDTO): Observable<RoomDTO> {
        /*const room: RoomDTO = {
            floorNumber: dto.floorNumber,
            description: dto.description,
        };*/


        console.log("",dto.name)
        console.log("",dto.buildingCode)
        console.log("",dto.floorNumber)
        console.log("",dto.description)
        console.log("",dto.category)
        console.log("",dto.dimensions)
        console.log("",dto.positions)

        const url = `${AppModule.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.floorNumber}/rooms`
        return this.http.post<RoomDTO>(url,
            {
                headers: { 'Content-type': 'application/json' },
                observe:'body',
                responseType:'json'
            })



    }


}
