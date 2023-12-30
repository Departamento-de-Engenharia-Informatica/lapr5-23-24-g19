import { HttpClient, HttpErrorResponse, HttpParams } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, catchError, from, switchMap, throwError } from 'rxjs'
import { Config } from '../config'
import { BuildingDTO } from '../dto/BuildingDTO'
import { CreateBuildingDTO } from '../dto/CreateBuildingDTO'
import { AuthService } from '@auth0/auth0-angular'
import { firstValueFrom } from 'rxjs'

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
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    getBuildings(): Observable<BuildingDTO[]> {
        const url = `${Config.baseUrl}/buildings`

        return new Observable<BuildingDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<BuildingDTO[]>(url, {
                            headers: { Authorization: `Bearer ${token}` },
                        })
                        .subscribe(
                            (buildings) => observer.next(buildings),
                            (error) => {
                                const errorMessage =
                                    error instanceof HttpErrorResponse
                                        ? error.error ||
                                          `An unexpected error occurred: ${error.message}`
                                        : 'An unexpected error occurred'
                                observer.error(new Error(errorMessage))
                            },
                            () => observer.complete(),
                        )
                })
                .catch((error) => observer.error(error))
        })
    }

    getBuildingsByFloors(dto: MinMaxDTO): Observable<BuildingByFloorsDTO[]> {
        let params = new HttpParams()
        params = params.set('minFloors', dto.min)
        params = params.set('maxFloors', dto.max)

        return new Observable<BuildingByFloorsDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<BuildingByFloorsDTO[]>(`${Config.baseUrl}/buildings/`, {
                            params,
                            observe: 'body',
                            headers: { Authorization: `Bearer ${token}` },
                            responseType: 'json',
                        })
                        .subscribe(
                            (buildings) => observer.next(buildings),
                            (error) => {
                                const errorMessage =
                                    error instanceof HttpErrorResponse
                                        ? error.error ||
                                          `An unexpected error occurred: ${error.message}`
                                        : 'An unexpected error occurred'
                                observer.error(new Error(errorMessage))
                            },
                            () => observer.complete(),
                        )
                })
                .catch((error) => observer.error(error))
        })
    }

    createBuilding(dto: CreateBuildingDTO) {
        return new Observable<BuildingDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<BuildingDTO>(
                            `${Config.baseUrl}/buildings`,
                            JSON.stringify(dto),
                            {
                                headers: {
                                    'Content-type': 'application/json',
                                    Authorization: `Bearer ${token}`,
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (building) => observer.next(building),
                            (error) => {
                                const errorMessage =
                                    error instanceof HttpErrorResponse
                                        ? error.error ||
                                          `An unexpected error occurred: ${error.message}`
                                        : 'An unexpected error occurred'
                                observer.error(new Error(errorMessage))
                            },
                            () => observer.complete(),
                        )
                })
                .catch((error) => observer.error(error))
        })
    }

    putBuilding(
        building: EditBuildingDTO,
        buildingCode: String,
    ): Observable<BuildingDTO> {
        return new Observable<BuildingDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .put<BuildingDTO>(
                            `${Config.baseUrl}/buildings/${buildingCode}`,
                            JSON.stringify(cleanObject(building)),
                            {
                                headers: {
                                    'Content-type': 'application/json',
                                    Authorization: `Bearer ${token}`,
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (building) => observer.next(building),
                            (error) => {
                                const errorMessage =
                                    error instanceof HttpErrorResponse
                                        ? error.error ||
                                          `An unexpected error occurred: ${error.message}`
                                        : 'An unexpected error occurred'
                                observer.error(new Error(errorMessage))
                            },
                            () => observer.complete(),
                        )
                })
                .catch((error) => observer.error(error))
        })
    }

    patchBuilding(
        building: EditBuildingDTO,
        buildingCode: String,
    ): Observable<BuildingDTO> {
        return new Observable<BuildingDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .patch<BuildingDTO>(
                            `${Config.baseUrl}/buildings/${buildingCode}`,
                            JSON.stringify(cleanObject(building)),
                            {
                                headers: {
                                    'Content-type': 'application/json',
                                    Authorization: `Bearer ${token}`,
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (building) => observer.next(building),
                            (error) => {
                                const errorMessage =
                                    error instanceof HttpErrorResponse
                                        ? error.error ||
                                          `An unexpected error occurred: ${error.message}`
                                        : 'An unexpected error occurred'
                                observer.error(new Error(errorMessage))
                            },
                            () => observer.complete(),
                        )
                })
                .catch((error) => observer.error(error))
        })
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
