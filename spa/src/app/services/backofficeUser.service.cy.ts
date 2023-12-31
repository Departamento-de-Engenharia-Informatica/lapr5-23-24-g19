// import { TestBed } from '@angular/core/testing'
// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { Config } from '../config'
// import { BackofficeUserService } from './backofficeUser.service'
// import { BackofficeUserDTO } from '../dto/BackofficeUserDTO'

// describe('BackofficeUserService: Unit Tests', () => {
//     let service: BackofficeUserService
//     let httpMock: HttpTestingController

//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//             providers: [BackofficeUserService],
//         })

//         service = TestBed.inject(BackofficeUserService)
//         httpMock = TestBed.inject(HttpTestingController)
//     })

//     afterEach(() => {
//         httpMock.verify()
//     })

//     describe('createBackofficeUser()', () => {
//         it('should create a backoffice user successfully', () => {
//             const dto: BackofficeUserDTO = {
//                 name: 'jonas',
//                 role: 'Fleet Manager',
//                 email: 'adsdasdasdasd@isep.ipp.pt',
//                 phoneNumber: '123755565',
//                 password: 'Jonasjonas124!',
//             }

//             const expectedBackofficeUser = {
//                 name: 'jonas',
//                 role: 'Fleet Manager',
//                 email: 'adsdasdasdasd@isep.ipp.pt',
//                 phoneNumber: '123755565',
//             }

//             service.createBackofficeUser(dto).subscribe((createdBackofficeUser) => {
//                 expect(createdBackofficeUser).to.eq(expectedBackofficeUser)
//             })

//             const req = httpMock.expectOne(`${Config.baseUrl}/users-backoffice`)
//             expect(req.request.method).to.eq('POST')

//             req.flush(expectedBackofficeUser)
//         })

//         it('should handle an error when creating a backoffice user', () => {
//             const dto: BackofficeUserDTO = {
//                 name: 'jonas',
//                 role: 'Fleet Manager',
//                 email: 'adsdasdasdasd@isep.ipp.pt',
//                 phoneNumber: '123755565',
//                 password: 'Jonasjonas124!',
//             }

//             const errorMessage = 'Error creating backoffice user'
//             const expectedError = new ErrorEvent(errorMessage)

//             service.createBackofficeUser(dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })

//             const req = httpMock.expectOne(`${Config.baseUrl}/users-backoffice`)
//             expect(req.request.method).to.eq('POST')

//             req.error(expectedError)
//         })
//     })
// })
