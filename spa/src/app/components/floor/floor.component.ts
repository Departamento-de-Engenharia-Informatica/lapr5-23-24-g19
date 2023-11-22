import { Component } from '@angular/core'
import { ErrorMessageService } from 'src/app/services/error-message.service'

@Component({
    selector: 'app-floor',
    templateUrl: './floor.component.html',
    styleUrls: ['./floor.component.css'],
})
export class FloorComponent {
    constructor(private errorsvc: ErrorMessageService) {}
    // TODO err messages
}
