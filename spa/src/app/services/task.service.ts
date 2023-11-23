import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { AppModule } from '../app.module';
import { HttpClient } from '@angular/common/http';
import { CriteriaDTO } from '../dto/CriteriaDTO';

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
  findRoute(): Observable<String>{
      alert('Funcionality not implemented.');
      return {} as Observable<String>
  }
}
