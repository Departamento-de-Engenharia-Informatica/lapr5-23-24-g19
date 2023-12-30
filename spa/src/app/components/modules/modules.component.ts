import { Component } from '@angular/core'
import { AppModule } from 'src/app/app.module'
import { UserService } from 'src/app/services/user.service'
import { RolesEnum } from 'src/app/services/user.service'
@Component({
    selector: 'app-modules',
    templateUrl: './modules.component.html',
    styleUrls: ['./modules.component.css'],
})
export class ModulesComponent {
    rolesEnum = RolesEnum

    constructor(public usr: UserService) {
        console.log(JSON.stringify(this.rolesEnum))
    }
    clickVisualization() {
        window.open(AppModule.visualizationUrl)
    }
}
