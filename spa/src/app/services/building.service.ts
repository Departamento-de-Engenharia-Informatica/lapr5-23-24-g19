import { HttpClientModule } from '@angular/common/http';
import { Injectable } from '@angular/core';
import {ActivatedRouteSnapshot, Resolve, RouterStateSnapshot} from '@angular/router'
import { Observable, throwError } from 'rxjs';
import { catchError, retry } from 'rxjs/operators';

interface BuildingProps{
  code:string
}

@Injectable({
  providedIn: 'root'
})
export class BuildingService{

  // constructor(private fetchService:FetchDataServ) {}



  // resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot) : Observable<BuildingProps> //| Promise<UserName> | UserName
  // { 
  //   return this.fetchService.getUser( route.params['id'] );
  // }
}
