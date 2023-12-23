import { Component, isDevMode } from '@angular/core'
import { EMPTY, Subscription, catchError, delay, interval, retry, retryWhen, switchMap } from 'rxjs'
import { AppModule } from './app.module'
import { HttpClient } from '@angular/common/http'
import { AuthService } from '@auth0/auth0-angular';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
})
export class AppComponent {
  title!: String
  isBackendUp!: boolean

  constructor(public auth: AuthService, private http: HttpClient) {
    this.auth.isAuthenticated$.subscribe(isAuthenticated => {
      if (isAuthenticated) {
        this.auth.getAccessTokenSilently().subscribe(token => {
          for (var i = 0; i < 10; i++) {
            console.log("Bearer ", token);
          }
        });
      }
    });

  }

  checkBackendStatus(): void {
    interval(3000)
      .pipe(
        switchMap(() =>
          this.http.get(`${AppModule.mdrUrl}/status`).pipe(
            catchError(() => {
              console.log('Backend is down')
              this.isBackendUp = false
              return EMPTY
            }),
          ),
        ),
        retry(), // Retry the entire observable sequence on error
        delay(3000), // Delay between retries
      )
      .subscribe(() => {
        this.isBackendUp = true
      })
  }


  ngOnInit(): void {
    this.title = 'RobDroneGo'
    this.isBackendUp = false
    this.checkBackendStatus()
    // TODO: ATIVAR LOGIN 
    // this.auth.isAuthenticated$.subscribe((loggedIn: boolean) => {
    //   if (!loggedIn) {
    //     // Redirect to Auth0 login
    //     this.auth.loginWithRedirect();
    //   }
    // });
  }
}
