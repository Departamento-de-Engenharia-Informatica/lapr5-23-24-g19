import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { ElevatorDTO } from 'src/app/dto/ElevatorDTO'
import { CreatedElevatorDTO } from 'src/app/dto/CreatedElevatorDTO'
import { FloorAndBuildingDTO, PatchFloorDTO, PutFloorDTO } from './floor.service'
import { Config } from '../config'

@Injectable()
export class ElevatorService {
    constructor(private http: HttpClient) {}

    createElevator(buildingId: string, dto: ElevatorDTO): Observable<CreatedElevatorDTO> {
        return this.http.post<CreatedElevatorDTO>(
            `${Config.baseUrl}/buildings/${buildingId}/elevators`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    getElevators(buildingCode: string): Observable<CreatedElevatorDTO[]> {
        const url = `${Config.baseUrl}/buildings/${buildingCode}/elevators`
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
            `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
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
            `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
