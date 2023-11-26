import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { RobotDTO } from 'src/app/dto/RobotDTO'
import { InhibitRobotDTO } from '../dto/InhibitRobotDTO'
import { RobotTypeRepo } from '../repos/RobotTypeRepo'
import { RobotRepo } from '../repos/RobotRepo'
import { RobotWithoutStateDTO } from '../dto/RobotWithoutStateDTO'
import { CreateRobotTypeDTO } from '../dto/CreateRobotTypeDTO'
import { Config } from '../config'

@Injectable({
    providedIn: 'root',
})
export class RobotService {
    constructor(
        private http: HttpClient,
        private robotRepo: RobotRepo,
        private robotTypeRepo: RobotTypeRepo,
    ) {}

    getRobots(): Observable<RobotDTO[]> {
        return this.http.get<RobotDTO[]>(`${Config.baseUrl}/robots`, {
            observe: 'body',
            responseType: 'json',
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

        return this.http.patch<RobotDTO>(
            `${Config.baseUrl}/robots/${code}/inhibit`,
            JSON.stringify(body),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
