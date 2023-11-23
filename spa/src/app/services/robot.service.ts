import { HttpClient } from '@angular/common/http'
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { AppModule } from '../app.module'
import { RobotDTO } from 'src/app/dto/RobotDTO'

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
