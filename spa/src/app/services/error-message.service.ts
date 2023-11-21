import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'

@Injectable({
    providedIn: 'root',
})
export class ErrorMessageService {
    private errorMessageSubject = new BehaviorSubject<string>('')
    errorMessage$ = this.errorMessageSubject.asObservable()
    setErrorMessage(message: string): void {
        console.log('set')
        this.errorMessageSubject.next(message)
    }
    private sucessMessageSubject = new BehaviorSubject<string>('')

    sucessMessage$ = this.sucessMessageSubject.asObservable()
    setSucessMessage(message: string): void {
        this.sucessMessageSubject.next(message)
    }
}
