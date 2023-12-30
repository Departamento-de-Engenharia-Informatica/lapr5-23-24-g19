import { Injectable } from '@angular/core'
import {
    CanActivate,
    ActivatedRouteSnapshot,
    RouterStateSnapshot,
    UrlTree,
    Router,
} from '@angular/router'
import { Observable } from 'rxjs'
import { UserService } from '../services/user.service' // Adjust the path as needed
import { RolesEnum } from '../services/user.service'

@Injectable({
    providedIn: 'root',
})
export class RoleAuthGuard implements CanActivate {
    constructor(private userService: UserService, private router: Router) {}

    canActivate(
        route: ActivatedRouteSnapshot,
        state: RouterStateSnapshot,
    ): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
        const requiredRole = route.data['requiredRole'] as RolesEnum[]

        if (this.userService.hasRole(requiredRole)) {
            return true
        } else {
            alert('Not authorized to perform this action')
            this.router.navigate(['/unauthorized'])
            return false
        }
    }
}
