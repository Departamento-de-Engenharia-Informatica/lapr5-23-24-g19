import { HttpClient, HttpResponse } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { AppModule } from '../app.module'
import {catchError, Observable, tap, throwError} from 'rxjs'

// interface BuildingProps{
//   code:string
// }

// @Injectable({
//   providedIn: 'root'
// })

export interface ElevatorDTO {
    floors: number[]
    brand?: string
    model?: string
    serialNumber?: string
    description?: string
}

export interface CreatedElevatorDTO {
    buildingId: string
    identifier: number
    floors: number[]

    brand?: string
    model?: string
    serialNumber?: string
    description?: string
}

@Injectable()
// {
// providedIn: AppModule
// }
export class ElevatorService {
    constructor(private http: HttpClient) {}

    createElevator(buildingId: string, dto: ElevatorDTO): Observable<CreatedElevatorDTO> {
        return this.http.post<CreatedElevatorDTO>(
            `${AppModule.baseUrl}/buildings/${buildingId}/elevators`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        ).pipe(
            catchError((error: any) => {
                if (error.status === 422) {
                    const errorMessage = error.error.message || 'Bad Request'
                    console.log(`Error: ${errorMessage}`)
                    return throwError(errorMessage)
                } else {
                    console.log(`Error: ${error.message}`)
                    return throwError(
                        'An unexpected error occurred. Please try again later.',
                    )
                }
            }),
        )
    }

    getElevators(buildingCode: string): Observable<CreatedElevatorDTO[]> {
        const url = `${AppModule.baseUrl}/buildings/${buildingCode}/elevators`
        return this.http.get<CreatedElevatorDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        })
    }
}
