import { Injectable } from '@angular/core'
import { Observable, catchError, throwError } from 'rxjs'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { CriterionDTO } from '../dto/CriteriaDTO'
import { TaskTypeDTO } from '../dto/CreateRobotTypeDTO'
import { Config } from '../config'
import { PathDTO } from '../dto/PathDTO'
import { GetPathsDTO } from '../dto/GetPathsDTO'
import { CreateDeliveryTaskDTO } from '../dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../dto/CreateSurveillanceTaskDTO'

@Injectable({
    providedIn: 'root',
})
export class TaskService {
    constructor(private http: HttpClient) {}

    getCriteria() {
        const url = `${Config.baseUrl}/paths/criteria`
        return this.http.get<CriterionDTO[]>(url, {
            observe: 'body',
            responseType: 'json',
        })
    }

    findRoute(dto: GetPathsDTO) {
        return this.http.post<PathDTO[]>(`${Config.baseUrl}/paths`, JSON.stringify(dto), {
            headers: { 'Content-type': 'application/json' },
            observe: 'body',
            responseType: 'json',
        })
    }

    tasksTypes(): Observable<TaskTypeDTO[]> {
        return this.http
            .get<TaskTypeDTO[]>(`${Config.baseUrl}/task/types`, {
                observe: 'body',
                responseType: 'json',
            })
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

    createSurveillanceTask(
        dto: CreateSurveillanceTaskDTO,
    ): Observable<CreateSurveillanceTaskDTO> {
        return this.http.post<CreateSurveillanceTaskDTO>(
            `${Config.baseUrl}/task/surveillance`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }

    createDeliveryTask(dto: CreateDeliveryTaskDTO): Observable<CreateDeliveryTaskDTO> {
        return this.http.post<CreateDeliveryTaskDTO>(
            `${Config.baseUrl}/task/delivery`,
            JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
