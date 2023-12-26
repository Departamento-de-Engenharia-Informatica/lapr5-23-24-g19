import { Injectable } from '@angular/core'
import { Observable, catchError, firstValueFrom, of, throwError } from 'rxjs'
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
import { AuthService } from '@auth0/auth0-angular'
import { ITaskAlgorithmDTO } from '../../../../mdr/src/dto/ITaskAlgorithmDTO'
import { IGeneralTaskDTO } from '../../../../mdr/src/dto/IGeneralTaskDTO'

@Injectable({
    providedIn: 'root',
})
export class TaskService {
    constructor(
        private http: HttpClient,
        private auth: AuthService,
    ) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    getApprovedTasks() {
        return new Observable<TaskDTO[]>((observer) => {
            // this.getToken()
            // .then((token) => {
            this.http
                .get<any[]>(`${Config.baseUrl}/task?status=approved`, {
                    headers: {
                        // Authorization: `Bearer ${token}`,
                        'Content-type': 'application/json',
                    },
                    observe: 'body',
                    responseType: 'json',
                })
                .subscribe(
                    (tasks) => {
                        const taskDtos = tasks.map((task) => {
                            return {
                                id: task.id.value,
                                requesterEmail: task.email,
                                requesterName: 'Nativo',
                                type: Object.values(TaskType)[task.jobType],
                                state: Object.values(TaskState)[task.status],
                            }
                        })
                        observer.next(taskDtos)
                        observer.complete()
                    },
                    (error) => {
                        observer.error(error)
                    },
                )
        })
        // .catch((error) => {
        //     observer.error(error)
        // })
        // })
    }

    sequenceTasks(dto: ITaskAlgorithmDTO) {
        return new Observable<TaskDTO[]>((observer) => {
            // this.getToken()
            //     .then((token) => {
            this.http
                .patch<TaskDTO[]>(
                    `${Config.baseUrl}/task/sequence`,
                    JSON.stringify(dto),
                    {
                        headers: {
                            // Authorization: `Bearer ${token}`,
                            'Content-type': 'application/json',
                        },
                        observe: 'body',
                        responseType: 'json',
                    },
                )
                .subscribe(
                    (tasks) => {
                        observer.next(tasks)
                        observer.complete()
                    },
                    (error) => {
                        observer.error(error)
                    },
                )
        })
        // .catch((error) => {
        //     observer.error(error)
        //         })
        // })
    }

    getCriteria() {
        return new Observable<CriterionDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<CriterionDTO[]>(`${Config.baseUrl}/paths/criteria`, {
                            headers: {
                                Authorization: `Bearer ${token}`,
                                'Content-type': 'application/json',
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (criteria) => {
                                observer.next(criteria)
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

    findRoute(dto: GetPathsDTO) {
        return new Observable<PathDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<PathDTO[]>(`${Config.baseUrl}/paths`, JSON.stringify(dto), {
                            headers: {
                                Authorization: `Bearer ${token}`,
                                'Content-type': 'application/json',
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (paths) => {
                                observer.next(paths)
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

    tasksTypes(): Observable<TaskTypeDTO[]> {
        return new Observable<TaskTypeDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<TaskTypeDTO[]>(`${Config.baseUrl}/task/types`, {
                            headers: {
                                Authorization: `Bearer ${token}`,
                                'Content-type': 'application/json',
                            },
                            observe: 'body',
                            responseType: 'json',
                        })
                        .subscribe(
                            (tasks) => {
                                observer.next(tasks)
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

    createSurveillanceTask(
        dto: CreateSurveillanceTaskDTO,
    ): Observable<CreateSurveillanceTaskDTO> {
        return new Observable<CreateSurveillanceTaskDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<CreateSurveillanceTaskDTO>(
                            `${Config.baseUrl}/task/surveillance`,
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
                            (createdSurveillanceTask) => {
                                observer.next(createdSurveillanceTask)
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

    createDeliveryTask(dto: CreateDeliveryTaskDTO): Observable<CreateDeliveryTaskDTO> {
        return new Observable<CreateDeliveryTaskDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .post<CreateDeliveryTaskDTO>(
                            `${Config.baseUrl}/task/delivery`,
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
                            (createdDeliveryTask) => {
                                observer.next(createdDeliveryTask)
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

    updateTask(dto: UpdateTaskDTO) {
        const { id: taskId, ...body } = dto

        console.log(taskId)
        console.log(body)

        // return of({})

        return new Observable<TaskDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .patch<TaskDTO>(
                            `${Config.baseUrl}/task/${taskId}`,
                            JSON.stringify(body),
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
                            (task) => {
                                observer.next(task)
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
            },
        ])
        //
        // return this.http.get<TaskDTO[]>(`${Config.baseUrl}/task?state=pending`, {
        //     observe: 'body',
        //     responseType: 'json',
        // })
    }

    getPendingTasks(): Observable<IGeneralTaskDTO[]> {
        return this.http.get<[]>(`${Config.baseUrl}/task?status=Pending`, {
            observe: 'body',
            responseType: 'json',
        })
    }

    getByCriteria(dto: FilterDTO): Observable<CreateDeliveryTaskDTO[]> {
        // console.log(
        //     `${Config.baseUrl}/task/filter?criteria=${dto.criteria}&rule=${dto.rule}`,
        // )
        // return this.http
        //     .get<CreateDeliveryTaskDTO[]>(
        //         `${Config.baseUrl}/task/filter?criteria=${dto.criteria}&rule=${dto.rule}`,
        //         {
        //             observe: 'body',
        //             responseType: 'json',
        //         },
        //     )
        //     .pipe(
        //         catchError((response: HttpErrorResponse) => {
        //             let errorMessage: string
        //
        //             if (response.error) {
        //                 errorMessage = response.error
        //             } else {
        //                 errorMessage = `An unexpected error occurred: ${response.message}`
        //             }
        //
        //             return throwError(() => new Error(errorMessage))
        //         }),
        //     )
        return new Observable<CreateDeliveryTaskDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<CreateDeliveryTaskDTO[]>(
                            `${Config.baseUrl}/task/filter?criteria=${dto.criteria}&rule=${dto.rule}`,
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
                            (tasks) => {
                                observer.next(tasks)
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
