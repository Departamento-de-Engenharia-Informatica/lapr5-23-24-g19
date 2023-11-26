import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { AppModule } from '../app.module'
import { RobotDTO } from 'src/app/dto/RobotDTO'
import { InhibitRobotDTO } from '../dto/InhibitRobotDTO'

@Injectable({
    providedIn: 'root',
})
export class RobotService {
    constructor(private http: HttpClient) {}

    getRobots(): Observable<RobotDTO[]> {
        return this.http.get<RobotDTO[]>(`${AppModule.baseUrl}/robots`, {
            observe: 'body',
            responseType: 'json',
        })
    }

    inhibit(dto: InhibitRobotDTO) {
        const code = dto.code
        const body = { state: dto.state }

        return this.http.patch<RobotDTO>(
            `${AppModule.baseUrl}/robots/${code}/inhibit`,
            JSON.stringify(body),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            },
        )
    }
}
