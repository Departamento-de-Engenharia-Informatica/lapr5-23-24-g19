import { ComponentFixture, TestBed } from '@angular/core/testing'
import { Observable } from 'rxjs'
import {BackofficeUserService, RoleDTO} from '../../../services/backofficeUser.service'
import { CreateBackofficeUserComponent } from './create-backoffice-user.component'
import { CreatedBackofficeUserDTO } from '../../../dto/CreatedBackofficeUserDTO'

describe('CreateBackofficeUserComponent: Unit Tests', () => {
    let backofficeUserServiceStub: Partial<BackofficeUserService>

    let component: CreateBackofficeUserComponent
    let fixture: ComponentFixture<CreateBackofficeUserComponent>

    const backofficeUser1: CreatedBackofficeUserDTO = {
        name: 'quim',
        role: 'Fleet Manager',
        email: 'joaquimfontesxtxo@isep.ipp.pt',
        phoneNumber: '123551265',
    }

    beforeEach(() => {
        backofficeUserServiceStub = {
            createBackofficeUser: function () {
                return new Observable<CreatedBackofficeUserDTO>((observer) => {
                    observer.next(backofficeUser1)
                    observer.complete()
                })
            },
            getRoles: function () {
                return new Observable<RoleDTO[]>((observer) => {
                    const roles: RoleDTO[] = [
                        { name: 'Fleet Manager' },
                        { name: 'Task Manager' },
                        { name: 'Campus Manager' },
                        { name: 'Administrator' },
                    ];
                    observer.next(roles);
                    observer.complete();
                })
            },

        }

        TestBed.configureTestingModule({
            declarations: [CreateBackofficeUserComponent],
            providers: [
                { provide: BackofficeUserService, useValue: backofficeUserServiceStub },
            ],
        })

        fixture = TestBed.createComponent(CreateBackofficeUserComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should create the component', () => {
        expect(component).to.exist
    })

    it('should create a backoffice user on form submission', () => {
        const createBackofficeUserSpy = cy.spy(
            component['service'],
            'createBackofficeUser',
        )
        const alertSpy = cy.spy(window, 'alert')
        const resetSpy = cy.spy(component.form, 'reset')

        component.form.setValue({
            name: 'quim',
            role: 'Fleet Manager',
            email: 'joaquimfontesxtxo@isep.ipp.pt',
            phoneNumber: '123551265',
            password: 'Jonasjonas123!',
            confirmPassword: 'Jonasjonas123!',
        })

        component.submit()

        expect(createBackofficeUserSpy).calledOnce
        expect(alertSpy).calledWith('Created user with success')
        expect(resetSpy).calledOnce
    })

    it('should reset form on successful backoffice user creation', () => {
        const resetSpy = cy.spy(component.form, 'reset')

        component.form.setValue({
            name: 'quim',
            role: 'Fleet Manager',
            email: 'joaquimfontesxtxo@isep.ipp.pt',
            phoneNumber: '123551265',
            password: 'Jonasjonas123!',
            confirmPassword: 'Jonasjonas123!',
        })

        component.submit()

        expect(resetSpy).calledOnce
    })
})
