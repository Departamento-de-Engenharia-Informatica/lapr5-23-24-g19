import { Observable, catchError, throwError } from "rxjs";
import { CreateRobotTypeDTO } from "../dto/CreateRobotTypeDTO";
import { HttpClient, HttpErrorResponse } from "@angular/common/http";
import { AppModule } from "../app.module";
import { Injectable } from "@angular/core";

interface RobotTypeDTO {
    code: string,
    brand: string,
    model: string,
    taskTypes: string[]
}

@Injectable({
    providedIn: 'root'
})
export class RobotTypeRepo {
    constructor(private http: HttpClient) { }

    createRobotType(dto: CreateRobotTypeDTO): Observable<RobotTypeDTO> {

        return this.http.post<RobotTypeDTO>(`${AppModule.baseUrl}/robottypes`, JSON.stringify(dto),
            {
                headers: { 'Content-type': 'application/json' },
                observe: 'body',
                responseType: 'json',
            }).pipe(
                catchError((response: HttpErrorResponse) => {
                    let errorMessage: string;

                    if (response.error) {
                        errorMessage = response.error
                    } else {
                        errorMessage = `An unexpected error occurred: ${response.message}`;
                    }

                    return throwError(() => new Error(errorMessage))
                })
            )

    }

}