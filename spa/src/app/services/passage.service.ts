import { HttpClient, HttpErrorResponse, HttpParams } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, catchError, throwError } from 'rxjs'
import { PassageDTO } from 'src/app/dto/PassageDTO'
import { BuildingCodePairDTO } from '../dto/BuildingCodePairDTO'
import { EditPassageDTO } from '../dto/EditPassageDTO'
import { Config } from '../config'

@Injectable({
    providedIn: 'root',
})
export class PassageService {
    constructor(private http: HttpClient) {}

    getPassagesBetweenBuildings(
        bCode1: string,
        bCode2: string,
    ): Observable<PassageDTO[]> {
        const url = `${Config.baseUrl}/passages/?building1=${bCode1}&building2=${bCode2}`
        return this.http.get<PassageDTO[]>(url, { observe: 'body', responseType: 'json' })
    }

    // Its the same as getPassagesBetweenBuildings but receives a dto instead
    getPassages(dto: BuildingCodePairDTO): Observable<PassageDTO[]> {
        let params = new HttpParams()
        params = params.set('building1', dto.buildingCode1)
        params = params.set('building2', dto.buildingCode2)

        return this.http.get<PassageDTO[]>(`${Config.baseUrl}/passages`, {
            params,
            observe: 'body',
            responseType: 'json',
        })
    }

    patchPassage(dto: EditPassageDTO): Observable<PassageDTO> {
        return this.http.patch<PassageDTO>(
            `${Config.baseUrl}/passages`,
            JSON.stringify(dto),

            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    postPassage(dto: PassageDTO): Observable<PassageDTO> {
        return this.http
            .post<PassageDTO>(`${Config.baseUrl}/passages`, JSON.stringify(dto), {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            })
            .pipe(
                catchError((response: HttpErrorResponse) => {
                    let errorMessage: string

                    if (response.error) {
                        // Extract the error message from the response body
                        errorMessage = response.error
                    } else {
                        // If there is no specific error message, use a generic one
                        errorMessage = `An unexpected error occurred: ${response.message}`
                    }

                    return throwError(() => new Error(errorMessage))
                }),
            )
    }
}
