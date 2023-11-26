import { Observable, catchError, throwError } from 'rxjs'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Config } from '../config'

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
    constructor(private http: HttpClient) {}

    createRobotType(dto: CreateRobotTypeDTO): Observable<CreateRobotTypeDTO> {
        return this.http
            .post<CreateRobotTypeDTO>(
                `${Config.baseUrl}/robottypes`,
                JSON.stringify(dto),
                {
                    headers: { 'Content-type': 'application/json' },
                    observe: 'body',
                    responseType: 'json',
                },
            )
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

    getRobotTypes(): Observable<CreateRobotTypeDTO[]> {
        return this.http.get<CreateRobotTypeDTO[]>(`${Config.baseUrl}/robottypes`, {
            headers: { 'Content-type': 'application/json' },
            observe: 'body',
            responseType: 'json',
        })
    }
}
