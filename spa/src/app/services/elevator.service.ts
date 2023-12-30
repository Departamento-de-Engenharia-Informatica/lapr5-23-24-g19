import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, firstValueFrom } from 'rxjs'
import { ElevatorDTO } from 'src/app/dto/ElevatorDTO'
import { CreatedElevatorDTO } from 'src/app/dto/CreatedElevatorDTO'
import { FloorAndBuildingDTO, PatchFloorDTO, PutFloorDTO } from './floor.service'
import { Config } from '../config'
import { EditElevatorDTO } from '../dto/EditElevatorDTO'
import { CreateElevatorDTO } from '../dto/CreateElevatorDTO'
import { AuthService } from '@auth0/auth0-angular'

@Injectable()
export class ElevatorService {
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    createElevator(
        buildingId: string,
        dto: CreateElevatorDTO,
    ): Observable<CreatedElevatorDTO> {
        return new Observable<CreatedElevatorDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<CreatedElevatorDTO>(
                            `${Config.baseUrl}/buildings/${buildingId}/elevators`,
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
                            (createdElevator) => {
                                observer.next(createdElevator)
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

    getElevators(buildingCode: string): Observable<CreatedElevatorDTO[]> {
        return new Observable<CreatedElevatorDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<CreatedElevatorDTO[]>(
                            `${Config.baseUrl}/buildings/${buildingCode}/elevators`,
                            {
                                headers: {
                                    Authorization: `Bearer ${token}`,
                                },
                                observe: 'body',
                                responseType: 'json',
                            },
                        )
                        .subscribe(
                            (elevators) => {
                                observer.next(elevators)
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

    patchElevator(dto: EditElevatorDTO): Observable<CreatedElevatorDTO> {
        const dtoElevator = {
            floors: dto.floors,
            brand: dto.brand ?? undefined,
            model: dto.model ?? undefined,
            serialNumber: dto.serialNumber ?? undefined,
            description: dto.description ?? undefined,
        } as ElevatorDTO

        return new Observable<CreatedElevatorDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .patch<CreatedElevatorDTO>(
                            `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
                            JSON.stringify(dtoElevator),
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
                            (createdElevator) => {
                                observer.next(createdElevator)
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

    putElevator(dto: EditElevatorDTO): Observable<CreatedElevatorDTO> {
        const dtoElevator = {
            floors: dto.floors,
            brand: dto.brand ?? undefined,
            model: dto.model ?? undefined,
            serialNumber: dto.serialNumber ?? undefined,
            description: dto.description ?? undefined,
        } as ElevatorDTO

        return new Observable<CreatedElevatorDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .put<CreatedElevatorDTO>(
                            `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
                            JSON.stringify(dtoElevator),
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
                            (createdElevator) => {
                                observer.next(createdElevator)
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
