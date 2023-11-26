import { HttpClient, HttpErrorResponse, HttpParams } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, catchError, throwError } from 'rxjs'
import { Config } from '../config'
import { BuildingDTO } from '../dto/BuildingDTO'

// interface BuildingProps{
//   code:string
// }

// @Injectable({
//   providedIn: 'root'
// })

// export interface BuildingDTO {
//     code: string
//     name: string
//     description: string
//     maxFloorDimensions: { length: number; width: number }
// }
export interface EditBuildingDTO {
    name?: string
    description?: string
    maxFloorDimensions?: { length: number; width: number }
}

export interface BuildingByFloorsDTO {
    code: string
    name?: string
    description?: string
    maxFloorDimensions: { length: number; width: number }
    floorNumber: number
}

export interface MinMaxDTO {
    min: number
    max: number
}

@Injectable()
export class BuildingService {
    constructor(private http: HttpClient) {}

    getBuildings(): Observable<BuildingDTO[]> {
        const url = `${Config.baseUrl}/buildings`
        return this.http
            .get<BuildingDTO[]>(url, {
                observe: 'body',
                responseType: 'json',
            })
            .pipe(
                catchError((response: HttpErrorResponse) => {
                    let errorMessage: string

                    if (response.error) {
                        errorMessage = response.error
                    } else {
                        errorMessage = `An unexpected error occurred: ${response.message}`
                    }

                    return throwError(() => new Error(errorMessage))
                }),
            )
    }

    getBuildingsByFloors(dto: MinMaxDTO): Observable<BuildingByFloorsDTO[]> {
        let params = new HttpParams()
        params = params.set('minFloors', dto.min)
        params = params.set('maxFloors', dto.max)

        return this.http.get<BuildingByFloorsDTO[]>(`${Config.baseUrl}/buildings/`, {
            params,
            observe: 'body',
            responseType: 'json',
        })
    }

    createBuilding(dto: BuildingDTO) {
        const building = cleanObject(dto)
        return this.http.post<BuildingDTO | undefined>(
            `${Config.baseUrl}/buildings`,
            JSON.stringify(building),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    putBuilding(
        building: EditBuildingDTO,
        buildingCode: String,
    ): Observable<BuildingDTO> {
        return this.http.put<BuildingDTO>(
            `${Config.baseUrl}/buildings/${buildingCode}`,
            JSON.stringify(cleanObject(building)),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    patchBuilding(
        building: EditBuildingDTO,
        buildingCode: String,
    ): Observable<BuildingDTO> {
        return this.http.patch<BuildingDTO>(
            `${Config.baseUrl}/buildings/${buildingCode}`,
            JSON.stringify(cleanObject(building)),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}

function cleanObject<T>(obj: T): T {
    const cleanedObj = {} as T

    for (const key in obj) {
        if (obj[key] !== null && obj[key] !== undefined && obj[key] != '') {
            cleanedObj[key] = obj[key]
        }
    }

    return cleanedObj
}
