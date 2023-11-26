import { Observable, catchError, throwError } from 'rxjs'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { AppModule } from '../app.module'
import { Injectable } from '@angular/core'

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
                `${AppModule.baseUrl}/robottypes`,
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
        return this.http.get<CreateRobotTypeDTO[]>(`${AppModule.baseUrl}/robottypes`, {
            headers: { 'Content-type': 'application/json' },
            observe: 'body',
            responseType: 'json',
        })
    }
}
