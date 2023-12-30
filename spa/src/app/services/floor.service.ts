import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, catchError, firstValueFrom, throwError } from 'rxjs'
import { FloorPassageDTO } from '../dto/FloorPassageDTO'
import { Config } from '../config'
import { AuthService } from '@auth0/auth0-angular'

export interface FloorAndBuildingDTO {
    buildingCode: string
    floorNumber: number
    description?: string
}

export interface PatchFloorDTO {
    buildingCode: string
    oldFloorNumber: number
    newFloorNumber?: number
    newDescription?: string
}

export interface PutFloorDTO {
    buildingCode: string
    oldFloorNumber: number
    newFloorNumber: number
    newDescription?: string
}

interface PatchDTO {
    floorNumber?: number
    description?: string
}

export interface UpdateMapDTO {
    map: string
}

interface FloorDTO {
    floorNumber: number
    description?: string
}

@Injectable({
    providedIn: 'root',
})
export class FloorService {
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    getFloors(buildingCode: string): Observable<FloorAndBuildingDTO[]> {
        return new Observable<FloorAndBuildingDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<FloorAndBuildingDTO[]>(
                            `${Config.baseUrl}/buildings/${buildingCode}/floors`,
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (floors) => {
                                observer.next(floors)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }

    createFloor(dto: FloorAndBuildingDTO): Observable<FloorAndBuildingDTO> {
        const floor: FloorDTO = {
            floorNumber: dto.floorNumber,
            description: dto.description,
        }

        // return this.http.post<FloorAndBuildingDTO>(
        //     `${Config.baseUrl}/buildings/${dto.buildingCode}/floors`,
        //     JSON.stringify(floor),
        //     {
        //         headers: { 'Content-type': 'application/json' },
        //         observe: 'body',
        //         responseType: 'json',
        //     },
        // )
        return new Observable<FloorAndBuildingDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<FloorAndBuildingDTO>(
                            `${Config.baseUrl}/buildings/${dto.buildingCode}/floors`,
                            JSON.stringify(floor),
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                    'Content-type': 'application/json',
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (floor) => {
                                observer.next(floor)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }

    patchFloor(dto: PatchFloorDTO): Observable<FloorAndBuildingDTO> {
        const edit: PatchDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        }

        return new Observable<FloorAndBuildingDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .patch<FloorAndBuildingDTO>(
                            `${Config.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
                            JSON.stringify(edit),
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                    'Content-type': 'application/json',
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (floor) => {
                                observer.next(floor)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }

    putFloor(dto: PutFloorDTO): Observable<FloorAndBuildingDTO> {
        const edit: FloorDTO = {
            floorNumber: dto.newFloorNumber,
            description: dto.newDescription,
        }

        return new Observable<FloorAndBuildingDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .put<FloorAndBuildingDTO>(
                            `${Config.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
                            JSON.stringify(edit),
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                    'Content-type': 'application/json',
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (floor) => {
                                observer.next(floor)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }

    updateMap(
        dto: UpdateMapDTO,
        buildingCode: String,
        floorNumber: number,
    ): Observable<UpdateMapDTO> {
        return new Observable<UpdateMapDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .patch<UpdateMapDTO>(
                            `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/map`,
                            JSON.stringify(dto),
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                    'Content-type': 'application/json',
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (dto) => {
                                observer.next(dto)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }

    getFloorsWithPassage(buildingCode: string): Observable<FloorPassageDTO[]> {
        return new Observable<FloorPassageDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<FloorPassageDTO[]>(
                            `${Config.baseUrl}/buildings/${buildingCode}/floors/passages`,
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (floors) => {
                                observer.next(floors)
                                observer.complete()
                            },
                            (error) => {
                                observer.error(error)
                            },
                        )
                })
                .catch((error) => {
                    observer.error(error)
                })
        })
    }
}

export { FloorPassageDTO }
