import { Observable, catchError, firstValueFrom, throwError } from 'rxjs'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Config } from '../config'
import { AuthService } from '@auth0/auth0-angular'

interface CreateRobotTypeDTO {
    code: string
    brand: string
    model: string
    taskTypes: string[]
}

@Injectable({
    providedIn: 'root',
})
export class RobotTypeRepo {
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    createRobotType(dto: CreateRobotTypeDTO): Observable<CreateRobotTypeDTO> {
        return new Observable<CreateRobotTypeDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<CreateRobotTypeDTO>(
                            `${Config.baseUrl}/robottypes`,
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
                            (createdRobotType) => {
                                observer.next(createdRobotType)
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

    getRobotTypes(): Observable<CreateRobotTypeDTO[]> {
        return new Observable<CreateRobotTypeDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<CreateRobotTypeDTO[]>(`${Config.baseUrl}/robottypes`, {
                            headers: {
                                Authorization: `Bearer ${token}`,
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (robotTypes) => {
                                observer.next(robotTypes)
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
