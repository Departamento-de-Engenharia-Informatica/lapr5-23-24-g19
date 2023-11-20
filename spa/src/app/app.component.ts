import { Component } from '@angular/core';
import { EMPTY, catchError, delay, interval, retry, retryWhen, switchMap } from 'rxjs';
import { AppModule } from './app.module';
import { HttpClient } from '@angular/common/http';

@Component({
  selector: 'app-root',
  templateUrl: "./app.component.html",
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title!:String
  isBackendUp!:boolean
  
  constructor(private http: HttpClient) { }
  
  ngOnInit(): void {
    this.title = 'RobDroneGo';
    this.isBackendUp = false
    this.checkBackendStatus();
  }
  
  checkBackendStatus(): void {
    interval(3000).pipe(
      switchMap(() => this.http.get(`${AppModule.mdrUrl}/status`).pipe(
        catchError(() => {
          console.log('Backend is down');
          this.isBackendUp=false
          return EMPTY;
        })
      )),
      retry(), // Retry the entire observable sequence on error
      delay(3000) // Delay between retries
    ).subscribe(() => {
      this.isBackendUp=true
    });
  }
}
