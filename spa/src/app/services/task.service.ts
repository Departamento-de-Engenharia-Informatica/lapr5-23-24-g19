import { Injectable } from '@angular/core'
import { Observable, catchError, of, throwError } from 'rxjs'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { CriterionDTO } from '../dto/CriteriaDTO'
import { TaskTypeDTO } from '../dto/CreateRobotTypeDTO'
import { Config } from '../config'
import { PathDTO } from '../dto/PathDTO'
import { GetPathsDTO } from '../dto/GetPathsDTO'
import { CreateDeliveryTaskDTO } from '../dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../dto/CreateSurveillanceTaskDTO'
import { FilterDTO } from '../dto/FilterDTO'
import { UpdateTaskDTO } from '../dto/UpdateTaskDTO'
import { TaskDTO, TaskState, TaskType } from '../dto/TaskDTO'

@Injectable({
    providedIn: 'root',
})
export class TaskService {
    constructor(private http: HttpClient) { }

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

    updateTask(dto: UpdateTaskDTO) {
        const { id: taskId, ...body } = dto

        console.log(taskId)
        console.log(body)

        // return of({})

        return this.http.patch<TaskDTO>(`${Config.baseUrl}/task/${taskId}`, JSON.stringify(body), {
            headers: { 'Content-type': 'application/json' },
            observe: 'body',
            responseType: 'json',
        })
    }

    // TODO: napoles & jonas
    pendingTasks() {
        return of<TaskDTO[]>([
            {
                id: 'ce7a98c5-683a-4c10-9f63-92eebbafc5cb',
                requesterEmail: '1210951@isep.ipp.pt',
                requesterName: 'Marco Maia',
                type: TaskType.SURVEILLANCE,
                state: TaskState.PENDING,
            },
            {
                id: 'e7a98c5-683a-4c10-9f63-92eebbafc5cb',
                requesterEmail: '1181478@isep.ipp.pt',
                requesterName: 'Jonas Antunes',
                type: TaskType.DELIVERY,
                state: TaskState.PENDING,
            },
            {
                id: 'db716712-e88f-49b1-8596-c7cfe7cd3d2a',
                requesterEmail: '1211155@isep.ipp.pt',
                requesterName: 'Jose Rente',
                type: TaskType.DELIVERY,
                state: TaskState.PENDING,
            }
        ])

        return this.http.get<TaskDTO[]>(`${Config.baseUrl}/task?state=pending`, {
            observe: 'body',
            responseType: 'json'
        })
    }

    getByCriteria(dto: FilterDTO): Observable<CreateDeliveryTaskDTO[]> {
        console.log(
            `${Config.baseUrl}/task/filter?criteria=${dto.criteria}&rule=${dto.rule}`,
        )
        return this.http
            .get<CreateDeliveryTaskDTO[]>(
                `${Config.baseUrl}/task/filter?criteria=${dto.criteria}&rule=${dto.rule}`,
                {
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
}
