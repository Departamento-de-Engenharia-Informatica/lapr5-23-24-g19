import { Injectable } from '@angular/core';
import { Observable, catchError, throwError } from 'rxjs';
import { AppModule } from '../app.module';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { CriteriaDTO } from '../dto/CriteriaDTO';
import { TaskTypeDTO } from '../dto/CreateRobotTypeDTO';

@Injectable({
  providedIn: 'root'
})
export class TaskService {
  constructor(private http: HttpClient) { }

  getCriterion(): Observable<CriteriaDTO[]> {
    const url = `${AppModule.baseUrl}/task/criterion`
    return this.http.get<CriteriaDTO[]>(url, {
      observe: 'body',
      responseType: 'json',
    })
  }

  findRoute(): Observable<String> {
    alert('Funcionality not implemented.');
    return {} as Observable<String>
  }


  tasksTypes(): Observable<TaskTypeDTO[]> {

    return this.http.get<TaskTypeDTO[]>(`${AppModule.baseUrl}/task/types`,
      {
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
