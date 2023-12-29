import { TestBed } from '@angular/core/testing'
import {
    HttpClientTestingModule,
    HttpTestingController,
} from '@angular/common/http/testing'
import { Config } from '../config'
import {BackofficeUserService} from "./backofficeUser.service";
import {BackofficeUserDTO} from "../dto/BackofficeUserDTO";
import {ClientService} from "./client.service";
import {ClientDTO} from "../dto/ClientDTO";

describe('ClientService: Unit Tests', () => {
    let service: ClientService
    let httpMock: HttpTestingController

    beforeEach(() => {
        TestBed.configureTestingModule({
            imports: [HttpClientTestingModule],
            providers: [ClientService],
        })

        service = TestBed.inject(ClientService)
        httpMock = TestBed.inject(HttpTestingController)
    })

    afterEach(() => {
        httpMock.verify()
    })

    describe('createClient()', () => {
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

            service
                .createClient(dto)
                .subscribe((createdClient) => {
                    expect(createdClient).to.eq(expectedClient)
                })

            const req = httpMock.expectOne(
                `${Config.baseUrl}/clients`,
            )
            expect(req.request.method).to.eq('POST')

            req.flush(expectedClient)
        })

        it('should handle an error when creating a backoffice user', () => {
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

            const req = httpMock.expectOne(
                `${Config.baseUrl}/clients`,
            )
            expect(req.request.method).to.eq('POST')

            req.error(expectedError)
        })
    })


})
