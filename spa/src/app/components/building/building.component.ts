import { Component } from '@angular/core'
import { ErrorMessageService } from 'src/app/services/error-message.service'

@Component({
    selector: 'app-building',
    templateUrl: './building.component.html',
    styleUrls: ['./building.component.css'],
})
export class BuildingComponent {
    constructor(private errorsvc: ErrorMessageService) {}
    // TODO err messages
}
