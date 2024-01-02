import { Injectable } from '@angular/core'
import {
    Observable,
    catchError,
    first,
    firstValueFrom,
    map,
    of,
    switchMap,
    throwError,
} from 'rxjs'
import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http'
import { CriterionDTO } from '../dto/CriteriaDTO'
import { TaskTypeDTO } from '../dto/CreateRobotTypeDTO'
import { Config } from '../config'
import { PathDTO } from '../dto/PathDTO'
import { GetPathsDTO } from '../dto/GetPathsDTO'
import { CreateDeliveryTaskDTO } from '../../../../mdr/src/dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../../../../mdr/src/dto/CreateSurveillanceTaskDTO'
import { FilterDTO } from '../dto/FilterDTO'
import { UpdateTaskDTO } from '../dto/UpdateTaskDTO'
import { TaskDTO, TaskState, TaskType } from '../dto/TaskDTO'
import { AuthService } from '@auth0/auth0-angular'
import { ITaskAlgorithmDTO } from '../../../../mdr/src/dto/ITaskAlgorithmDTO'
import { IGeneralTaskDTO } from '../../../../mdr/src/dto/IGeneralTaskDTO'
import { SequenceAlgorithmDTO } from '../dto/SequenceAlgorithmDTO'
import { RobotSequenceDTO } from '../dto/RobotSequenceDTO'

@Injectable({
    providedIn: 'root',
})
export class TaskService {
    constructor(private http: HttpClient, private auth: AuthService) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    private getTokenObservable(): Observable<string> {
        return this.auth.getAccessTokenSilently().pipe(
            first(), // Take the first emitted value and complete the observable
            catchError((error) => {
                console.error('Error getting token:', error)
                return throwError(() => new Error('Unable to get authentication token.'))
            }),
        )
    }

    // TODO: change usages of _fakeToken() to getTokenObservable()
    private _fakeToken() {
        return of('aa')
    }

    private authHeaders(token: string): HttpHeaders {
        return new HttpHeaders({
            Authorization: `Bearer ${token}`,
        })
    }

    tasksOfState(state: TaskState): Observable<TaskDTO[]> {
        return this.getTokenObservable().pipe(
            switchMap((token) =>
                this.http.get<IGeneralTaskDTO[]>(
                    `${Config.baseUrl}/task?status=${state}`,
                    {
                        headers: this.authHeaders(token),
                        observe: 'body',
                        responseType: 'json',
                    },
                ),
            ),
            map((tasks) =>
                tasks.map(
                    (t): TaskDTO => ({
                        ...t,
                        id: t.id.value,
                        type: Object.values(TaskType)[t.jobType],
                        state: Object.values(TaskState)[t.status],

                        requesterEmail: t.email,
                        requesterName:
                            t.surveillanceContact?.name ??
                            t.pickupContact?.name ??
                            'Unknown',
                    }),
                ),
            ),
            catchError((err) => throwError(() => err)),
        )
    }

    getApprovedTasks() {
        return this.tasksOfState(TaskState.APPROVED)
    }

    taskSequenceAlgorithms(): Observable<SequenceAlgorithmDTO[]> {
        return this.getTokenObservable().pipe(
            switchMap((token) =>
                this.http.get<SequenceAlgorithmDTO[]>(
                    `${Config.baseUrl}/task/sequence/algorithms`,
                    {
                        headers: this.authHeaders(token),
                        observe: 'body',
                        responseType: 'json',
                    },
                ),
            ),
            catchError((err) => throwError(() => err)),
        )
    }

    sequenceTasks(dto: ITaskAlgorithmDTO) {
        return this.getTokenObservable().pipe(
            switchMap((token) => {
                return this.http.patch<RobotSequenceDTO[]>(
                    `${Config.baseUrl}/task/sequence`,
                    JSON.stringify(dto),
                    {
                        headers: this.authHeaders(token).set(
                            'Content-Type',
                            'application/json',
                        ),
                        observe: 'body',
                        responseType: 'json',
                    },
                )
            }),
            catchError((error) => throwError(() => error)),
        )
        // return new Observable<TaskDTO[]>((observer) => {
        //     // this.getToken()
        //     //     .then((token) => {
        //     this.http
        //         .patch<TaskDTO[]>(
        //             `${Config.baseUrl}/task/sequence`,
        //             JSON.stringify(dto),
        //             {
        //                 headers: {
        //                     // Authorization: `Bearer ${token}`,
        //                     'Content-type': 'application/json',
        //                 },
        //                 observe: 'body',
        //                 responseType: 'json',
        //             },
        //         )
        //         .subscribe(
        //             (tasks) => {
        //                 observer.next(tasks)
        //                 observer.complete()
        //             },
        //             (error) => {
        //                 observer.error(error)
        //             },
        //         )
        // })
        // // .catch((error) => {
        // //     observer.error(error)
        // //         })
        // // })
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

        return this.getTokenObservable().pipe(
            switchMap((token) => {
                const headers = this.authHeaders(token).set(
                    'Content-type',
                    'application/json',
                )

                return this.http.patch<TaskDTO>(
                    `${Config.baseUrl}/task/${taskId}`,
                    JSON.stringify(body),
                    { headers, observe: 'body', responseType: 'json' },
                )
            }),
        )
    }

    getPendingTasks(): Observable<IGeneralTaskDTO[]> {
        // return this.http.get<[]>(`${Config.baseUrl}/task?status=Pending`, {
        //     observe: 'body',
        //     responseType: 'json',
        // })
        return new Observable<IGeneralTaskDTO[]>((observer) => {
            this.getToken().then((token) => {
                this.http
                    .get<IGeneralTaskDTO[]>(`${Config.baseUrl}/task?status=Pending`, {
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
        })
    }

    getByCriteria(dto: FilterDTO): Observable<IGeneralTaskDTO[]> {
        return new Observable<IGeneralTaskDTO[]>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .get<IGeneralTaskDTO[]>(
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
