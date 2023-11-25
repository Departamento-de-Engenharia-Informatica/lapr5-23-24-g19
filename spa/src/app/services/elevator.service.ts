import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { AppModule } from '../app.module'
import { Observable } from 'rxjs'
import { ElevatorDTO } from 'src/app/dto/ElevatorDTO'
import { CreatedElevatorDTO } from 'src/app/dto/CreatedElevatorDTO'
import {FloorAndBuildingDTO, PatchFloorDTO, PutFloorDTO} from "./floor.service";

@Injectable()
// {
// providedIn: AppModule
// }
export class ElevatorService {
    constructor(private http: HttpClient) {}

    createElevator(buildingId: string, dto: ElevatorDTO): Observable<CreatedElevatorDTO> {
        return this.http.post<CreatedElevatorDTO>(
            `${AppModule.baseUrl}/buildings/${buildingId}/elevators`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    getElevators(buildingCode: string): Observable<CreatedElevatorDTO[]> {
        const url = `${AppModule.baseUrl}/buildings/${buildingCode}/elevators`
        return this.http.get<CreatedElevatorDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        })
    }

    patchElevator(dto: CreatedElevatorDTO): Observable<CreatedElevatorDTO> {
        /*const edit: PatchDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        }*/

        return this.http.patch<CreatedElevatorDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    putElevator(dto: CreatedElevatorDTO): Observable<CreatedElevatorDTO> {
        /*const edit: FloorDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        }*/
        return this.http.put<CreatedElevatorDTO>(
            `${AppModule.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
