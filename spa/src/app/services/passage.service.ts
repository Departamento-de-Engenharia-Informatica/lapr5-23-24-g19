import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, throwError } from 'rxjs';
import { AppModule } from "../app.module";

export interface PassageDTO {
    floor1: {
        buildingCode: string
        floorNumber: number
    }
    floor2: {
        buildingCode: string
        floorNumber: number
    }
}


@Injectable({
    providedIn: 'root',
})
export class PassageService {
    constructor(private http: HttpClient) { }

    getPassagesBetweenBuildings(bCode1: string, bCode2: string): Observable<PassageDTO[]> {
        const url = `${AppModule.baseUrl}/passages/?building1=${bCode1}&building2=${bCode2}`
        return this.http.get<PassageDTO[]>(url, { observe: 'body', responseType: 'json' })
    }

    postPassage(dto: PassageDTO): Observable<PassageDTO> {
        console.log("post")
        return this.http.post<PassageDTO>(
          `${AppModule.baseUrl}/passages`,
          JSON.stringify(dto),
          {
            headers: { 'Content-type': 'application/json' },
            observe: 'body',
            responseType: 'json',
          },
        ).pipe(
          catchError((error: any) => {
            // Handle error here
            console.log("deu erro")
            if (error.status === 422) {
              const errorMessage = error.error.message || 'Bad Request';
              console.log(`Error: ${errorMessage}`);
              return throwError(errorMessage);
            } else {
              console.log(`Error: ${error.message}`);
              return throwError('An unexpected error occurred. Please try again later.');
            }
          })
        );
      }
      
}
