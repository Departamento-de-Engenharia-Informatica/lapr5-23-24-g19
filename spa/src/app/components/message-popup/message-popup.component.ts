import { Component, OnInit } from '@angular/core'
import { ErrorMessageService } from 'src/app/services/error-message.service'

@Component({
    selector: 'app-error-popup',
    templateUrl: './message-popup.component.html',
    styleUrls: ['./message-popup.component.css'],
})
export class MessagePopupComponent implements OnInit {
    showError: boolean
    showSucess: boolean
    errorMessage!: string
    sucessMessage!: string

    constructor(private messageService: ErrorMessageService) {
        this.showError = false
        this.showSucess = false
    }

    ngOnInit(): void {
        this.showError = false
        this.showSucess = false
        this.messageService.errorMessage$.subscribe((message) => {
            this.errorMessage = message
            this.openErrorPopUp()
        })

        this.messageService.sucessMessage$.subscribe((message) => {
            this.sucessMessage = message
            this.openSucessPopUp()
        })
    }

    openErrorPopUp() {
        this.showError = true
    }

    closeErrorForm() {
        this.showError = false
    }

    popErrorShow(): boolean {
        return this.showError
    }

    openSucessPopUp() {
        this.showSucess = true
    }

    closeSucessForm() {
        this.showSucess = false
    }

    popSucessShow(): boolean {
        return this.showSucess
    }
}
