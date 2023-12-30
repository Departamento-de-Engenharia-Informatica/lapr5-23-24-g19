import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, firstValueFrom } from 'rxjs'
import { Config } from '../config'
import { AuthService } from '@auth0/auth0-angular'

interface RobotWithoutStateDTO {
    code: string
    nickname: string
    typeCode: string
    serialNumber: string
    description?: string
}

interface RobotDTO {
    code: string
    nickname: string
    typeCode: string
    serialNumber: string
    description?: string
    state: number
}

@Injectable({
    providedIn: 'root',
})
export class RobotRepo {
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    createRobot(dto: RobotWithoutStateDTO): Observable<RobotDTO> {
        // return this.http.post<RobotDTO>(`${Config.baseUrl}/robots`, JSON.stringify(dto), {
        //     headers: { 'Content-type': 'application/json' },
        //     observe: 'body',
        //     responseType: 'json',
        // })
        return new Observable<RobotDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<RobotDTO>(`${Config.baseUrl}/robots`, JSON.stringify(dto), {
                            headers: {
                                Authorization: `Bearer ${token}`,
                                'Content-type': 'application/json',
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (robot) => {
                                observer.next(robot)
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

    getRobots(): Observable<RobotDTO[]> {
        // return this.http.get<RobotDTO[]>(`${Config.baseUrl}/robots`, {
        //     headers: { 'Content-type': 'application/json' },
        //     observe: 'body',
        //     responseType: 'json',
        // })
        return new Observable<RobotDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<RobotDTO[]>(`${Config.baseUrl}/robots`, {
                            headers: {
                                Authorization: `Bearer ${token}`,
                                'Content-type': 'application/json',
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (robots) => {
                                observer.next(robots)
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
