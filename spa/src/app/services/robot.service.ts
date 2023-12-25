import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable, firstValueFrom } from 'rxjs'
import { RobotDTO } from 'src/app/dto/RobotDTO'
import { InhibitRobotDTO } from '../dto/InhibitRobotDTO'
import { RobotTypeRepo } from '../repos/RobotTypeRepo'
import { RobotRepo } from '../repos/RobotRepo'
import { RobotWithoutStateDTO } from '../dto/RobotWithoutStateDTO'
import { CreateRobotTypeDTO } from '../dto/CreateRobotTypeDTO'
import { Config } from '../config'
import { AuthService } from '@auth0/auth0-angular'

@Injectable({
    providedIn: 'root',
})
export class RobotService {
    constructor(
        private http: HttpClient,
        private robotRepo: RobotRepo,
        private robotTypeRepo: RobotTypeRepo,
        private auth: AuthService,
    ) {}

    async getToken(): Promise<string> {
        const tokenObservable = this.auth.getAccessTokenSilently()
        const token = await firstValueFrom(tokenObservable)
        return token
    }

    getRobots(): Observable<RobotDTO[]> {
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

    getRobotsOnion(): Observable<RobotDTO[]> {
        return this.robotRepo.getRobots()
    }

    getRobotTypes(): Observable<CreateRobotTypeDTO[]> {
        return this.robotTypeRepo.getRobotTypes()
    }

    createRobot(dto: RobotWithoutStateDTO): Observable<RobotDTO> {
        return this.robotRepo.createRobot(dto)
    }

    inhibit(dto: InhibitRobotDTO) {
        const code = dto.code
        const body = { state: dto.state }

        return new Observable<RobotDTO>((observer) => {
            this.getToken()
                .then((token) => {
                    this.http
                        .patch<RobotDTO>(
                            `${Config.baseUrl}/robots/${code}/inhibit`,
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
}
