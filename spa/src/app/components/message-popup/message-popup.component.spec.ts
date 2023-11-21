import { ComponentFixture, TestBed } from '@angular/core/testing'

import { MessagePopupComponent } from './message-popup.component'

describe('ErrorPopupComponent', () => {
    let component: MessagePopupComponent
    let fixture: ComponentFixture<MessagePopupComponent>

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [MessagePopupComponent],
        })
        fixture = TestBed.createComponent(MessagePopupComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create', () => {
        expect(component).toBeTruthy()
    })
})
