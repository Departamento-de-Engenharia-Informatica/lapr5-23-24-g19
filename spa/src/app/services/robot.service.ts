import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { AppModule } from '../app.module'

export interface RobotDTO {
    code: string
    nickname: string
    typeCode: string
    serialNumber: string
    description?: string
}

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
}
