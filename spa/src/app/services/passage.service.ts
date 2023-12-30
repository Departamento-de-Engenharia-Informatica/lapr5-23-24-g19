import { HttpClient, HttpErrorResponse, HttpParams } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, catchError, firstValueFrom, throwError } from 'rxjs'
import { PassageDTO } from 'src/app/dto/PassageDTO'
import { BuildingCodePairDTO } from '../dto/BuildingCodePairDTO'
import { EditPassageDTO } from '../dto/EditPassageDTO'
import { Config } from '../config'
import { AuthService } from '@auth0/auth0-angular'

@Injectable({
    providedIn: 'root',
})
export class PassageService {
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    getPassagesBetweenBuildings(
        bCode1: string,
        bCode2: string,
    ): Observable<PassageDTO[]> {
        return new Observable<PassageDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    let params = new HttpParams()
                    params = params.set('building1', bCode1)
                    params = params.set('building2', bCode2)
                    this.http
                        .get<PassageDTO[]>(`${Config.baseUrl}/passages`, {
                            params,
                            headers: {
                                Authorization: `Bearer ${token}`,
                                'Content-type': 'application/json',
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (passages) => {
                                observer.next(passages)
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

    // Its the same as getPassagesBetweenBuildings but receives a dto instead
    getPassages(dto: BuildingCodePairDTO): Observable<PassageDTO[]> {
        let params = new HttpParams()
        params = params.set('building1', dto.buildingCode1)
        params = params.set('building2', dto.buildingCode2)

        return new Observable<PassageDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<PassageDTO[]>(`${Config.baseUrl}/passages`, {
                            params,
                            headers: {
                                Authorization: `Bearer ${token}`,
                                'Content-type': 'application/json',
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (passages) => {
                                observer.next(passages)
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

    patchPassage(dto: EditPassageDTO): Observable<PassageDTO> {
        return new Observable<PassageDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .patch<PassageDTO>(
                            `${Config.baseUrl}/passages`,
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
                            (passage) => {
                                observer.next(passage)
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

    postPassage(dto: PassageDTO): Observable<PassageDTO> {
        return new Observable<PassageDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<PassageDTO>(
                            `${Config.baseUrl}/passages`,
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
                            (passage) => {
                                observer.next(passage)
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
