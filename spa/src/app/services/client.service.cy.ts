import { TestBed } from '@angular/core/testing'
import {
    HttpClientTestingModule,
    HttpTestingController,
} from '@angular/common/http/testing'
import { Config } from '../config'
import { BackofficeUserService } from './backofficeUser.service'
import { BackofficeUserDTO } from '../dto/BackofficeUserDTO'
import { ClientService } from './client.service'
import { ClientDTO } from '../dto/ClientDTO'
import { ClientEmailDTO } from '../dto/ClientEmailDTO'
import { CreatedClientDTO } from '../dto/CreatedClientDTO'
import {AuthService} from "@auth0/auth0-angular";
import {of} from "rxjs";

describe('ClientService: Unit Tests', () => {
    let service: ClientService
    let httpMock: HttpTestingController

    const authMock = {
        getAccessTokenSilently: () => of('mock-token'), // Returns a mock token
    };

    beforeEach(() => {
        TestBed.configureTestingModule({
            imports: [HttpClientTestingModule],
            providers: [ClientService,{ provide: AuthService, useValue: authMock }],
        })

        service = TestBed.inject(ClientService)
        httpMock = TestBed.inject(HttpTestingController)
    })

    afterEach(() => {
        httpMock.verify()
    })

    describe('Client()', () => {
        it('should create a client successfully', () => {
            const dto: ClientDTO = {
                name: 'jonas',
                email: 'adsdasdsadsadsdasd@isep.ipp.pt',
                phoneNumber: '122255565',
                vatNumber: 123722565,
                password: 'Jonasjonas124!',
            }

            const expectedClient = {
                name: 'jonas',
                email: 'adsdasdsadsadsdasd@isep.ipp.pt',
                phoneNumber: '122255565',
                vatNumber: 123722565,
                status: 'Pending',
            }

            service.createClient(dto).subscribe((createdClient) => {
                expect(createdClient).to.eq(expectedClient)
            })

            const req = httpMock.expectOne(`${Config.baseUrl}/clients`)
            expect(req.request.method).to.eq('POST')

            req.flush(expectedClient)
        })

        it('should handle an error when creating a client user', () => {
            const dto: ClientDTO = {
                name: 'jonas',
                email: 'adsdasdsadsadsdasd@isep.ipp.pt',
                phoneNumber: '122255565',
                vatNumber: 123722565,
                password: 'Jonasjonas124!',
            }

            const errorMessage = 'Error creating client'
            const expectedError = new ErrorEvent(errorMessage)

            service.createClient(dto).subscribe({
                error: (error) => {
                    expect(error.message).to.eq(errorMessage)
                },
            })

            const req = httpMock.expectOne(`${Config.baseUrl}/clients`)
            expect(req.request.method).to.eq('POST')

            req.error(expectedError)
        })

        it('should get a client successfully', () => {
            const email = 'adsdasdsadsadsdasd@isep.ipp.pt'

            const expectedClient = {
                name: 'jonas',
                email: 'adsdasdsadsadsdasd@isep.ipp.pt',
                phoneNumber: '122255565',
                vatNumber: 123722565,
            }

            service.getClient(email).subscribe((getClient) => {
                expect(getClient).to.eq(expectedClient)
            })

            const req = httpMock.expectOne(`${Config.baseUrl}/clients/${email}`)
            expect(req.request.method).to.eq('GET')

            req.flush(expectedClient)
        })

        it('should get pending  clients ', () => {
            const expectedClients: CreatedClientDTO[] = [
                {
                    name: 'jonas',
                    email: 'adsdasdsadsadsdasd@isep.ipp.pt',
                    phoneNumber: '122255565',
                    vatNumber: 123722565,
                    status: 'Pending',
                },
                {
                    name: 'quim',
                    email: 'adsdasadsadasdasdasdasddsadsadsdasd@isep.ipp.pt',
                    phoneNumber: '111255565',
                    vatNumber: 123721165,
                    status: 'Pending',
                },
            ]

            service.getPendingClients().subscribe((getClient) => {
                expect(getClient).to.eq(expectedClients)
            })

            const req = httpMock.expectOne(`${Config.baseUrl}/clients?state=Pending`)
            expect(req.request.method).to.eq('GET')

            req.flush(expectedClients)
        })

        it('should delete a client successfully', () => {
            const dto: ClientEmailDTO = {
                email: 'adsdasdsadsadsdasd@isep.ipp.pt',
            }

            const expectedClient = {
                email: 'adsdasdsadsadsdasd@isep.ipp.pt',
            }

            service.deleteClient(dto).subscribe((deletedClient) => {
                expect(deletedClient).to.eq(expectedClient)
            })

            const req = httpMock.expectOne(`${Config.baseUrl}/clients/${dto.email}`)
            expect(req.request.method).to.eq('DELETE')

            req.flush(expectedClient)
        })
    })
})
