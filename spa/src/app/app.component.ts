import { Component } from '@angular/core';
import { catchError, interval, switchMap } from 'rxjs';
import { AppModule } from './app.module';
import { HttpClient } from '@angular/common/http';

@Component({
  selector: 'app-root',
  templateUrl: "./app.component.html",
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'RobDroneGo';
  baseUrl = ''
  isBackendUp=true

  constructor(private http: HttpClient) { }

  ngOnInit(): void {
    // this.checkBackendStatus();
  }

  // checkBackendStatus(): void {
  //   interval(3000).pipe(
  //     switchMap(() => this.http.get(`${AppModule.mdrUrl}/status`)),
  //     catchError(() => {
  //       console.log('Backend is down');
  //       // this.isBackendUp = false
  //       return [];
  //     })
  //   ).subscribe(() => {
  //     console.log('Backend is up');
  //     // this.isBackendUp = true
  //   }
  //   )
  // }
}
