import { Injectable } from '@angular/core'
import { CreateRobotTypeDTO } from '../dto/CreateRobotTypeDTO'
import { RobotTypeRepo } from '../repos/RobotTypeRepo'
import { Observable, catchError, map, throwError } from 'rxjs'

@Injectable({
    providedIn: 'root',
})
export class RobotTypeService {
    constructor(private repo: RobotTypeRepo) {}

    createRobotType(dto: CreateRobotTypeDTO): Observable<string> {
        return this.repo.createRobotType(dto).pipe(
            map(() => {
                return 'Robot created successfully!'
            }),
            catchError((error) => {
                return throwError(error.message)
            }),
        )
    }
}
