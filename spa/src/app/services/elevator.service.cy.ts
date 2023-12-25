// import { TestBed } from '@angular/core/testing'
// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { ElevatorService } from './elevator.service'
// import { CreateElevatorDTO } from '../dto/CreateElevatorDTO'
// import { EditElevatorDTO } from '../dto/EditElevatorDTO'
// import { Config } from '../config'
// import { CreatedElevatorDTO } from '../dto/CreatedElevatorDTO'
//
// describe('ElevatorService: Unit Tests', () => {
//     let service: ElevatorService
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//             providers: [ElevatorService],
//         })
//
//         service = TestBed.inject(ElevatorService)
//         httpMock = TestBed.inject(HttpTestingController)
//     })
//
//     afterEach(() => {
//         httpMock.verify()
//     })
//
//     describe('createElevator()', () => {
//         it('should create an elevator successfully', () => {
//             const buildingId = 'B001'
//             const dto: CreateElevatorDTO = {
//                 floors: [1, 2, 3, 4, 5],
//                 brand: 'BrandX',
//                 model: 'ModelY',
//                 serialNumber: 'SN123',
//                 description: 'A new elevator',
//             }
//
//             const expectedElevator = {
//                 buildingId,
//                 identifier: 1,
//                 floors: [1, 2, 3, 4, 5],
//                 brand: dto.brand,
//                 model: dto.model,
//                 serialNumber: dto.serialNumber,
//                 description: dto.description,
//             }
//
//             service.createElevator(buildingId, dto).subscribe((createdElevator) => {
//                 expect(createdElevator).to.eq(expectedElevator)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingId}/elevators`,
//             )
//             expect(req.request.method).to.eq('POST')
//
//             req.flush(expectedElevator)
//         })
//
//         it('should handle an error when creating an elevator', () => {
//             const buildingId = 'B001'
//             const dto: CreateElevatorDTO = {
//                 floors: [1, 2, 3, 4, 5],
//                 brand: 'BrandX',
//                 model: 'ModelY',
//                 serialNumber: 'SN123',
//                 description: 'A new elevator',
//             }
//
//             const errorMessage = 'Error creating elevator'
//             const expectedError = new ErrorEvent(errorMessage)
//
//             service.createElevator(buildingId, dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingId}/elevators`,
//             )
//             expect(req.request.method).to.eq('POST')
//
//             req.error(expectedError)
//         })
//     })
//
//     describe('getElevators()', () => {
//         it('should get elevators successfully', () => {
//             const buildingCode = 'B001'
//             const expectedElevators: CreatedElevatorDTO[] = [
//                 {
//                     buildingId: buildingCode,
//                     identifier: 1,
//                     floors: [1, 2, 3],
//                     brand: 'BrandX',
//                     model: 'ModelY',
//                     serialNumber: 'SN123',
//                     description: 'Elevator 1',
//                 },
//             ]
//
//             service.getElevators(buildingCode).subscribe((elevators) => {
//                 expect(elevators).to.eq(expectedElevators)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/elevators`,
//             )
//             expect(req.request.method).to.eq('GET')
//
//             req.flush(expectedElevators)
//         })
//
//         it('should handle an error when getting elevators', () => {
//             const buildingCode = 'B001'
//             const errorMessage = 'Error getting elevators'
//             const expectedError = new ErrorEvent(errorMessage)
//
//             service.getElevators(buildingCode).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/elevators`,
//             )
//             expect(req.request.method).to.eq('GET')
//
//             req.error(expectedError)
//         })
//     })
//
//     describe('patchElevator()', () => {
//         it('should patch an elevator successfully', () => {
//             const dto: EditElevatorDTO = {
//                 buildingId: 'B001',
//                 identifier: 1,
//                 floors: [1, 2, 3],
//                 brand: 'BrandX',
//                 model: 'ModelY',
//                 serialNumber: 'SN123',
//                 description: 'Updated elevator description',
//             }
//
//             const expectedElevator: CreatedElevatorDTO = {
//                 buildingId: dto.buildingId,
//                 identifier: dto.identifier,
//                 floors: dto.floors,
//                 brand: dto.brand,
//                 model: dto.model,
//                 serialNumber: dto.serialNumber,
//                 description: dto.description,
//             }
//
//             service.patchElevator(dto).subscribe((patchedElevator) => {
//                 expect(patchedElevator).to.eq(expectedElevator)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
//             )
//             expect(req.request.method).to.eq('PATCH')
//
//             req.flush(expectedElevator)
//         })
//
//         it('should handle an error when patching an elevator', () => {
//             const dto: EditElevatorDTO = {
//                 buildingId: 'B001',
//                 identifier: 1,
//                 floors: [1, 2, 3],
//                 brand: 'BrandX',
//                 model: 'ModelY',
//                 serialNumber: 'SN123',
//                 description: 'Updated elevator description',
//             }
//
//             const errorMessage = 'Error patching elevator'
//             const expectedError = new ErrorEvent(errorMessage)
//
//             service.patchElevator(dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
//             )
//             expect(req.request.method).to.eq('PATCH')
//
//             req.error(expectedError)
//         })
//     })
//
//     describe('putElevator()', () => {
//         it('should put an elevator successfully', () => {
//             const dto: EditElevatorDTO = {
//                 buildingId: 'B001',
//                 identifier: 1,
//                 floors: [1, 2, 3],
//                 brand: 'BrandX',
//                 model: 'ModelY',
//                 serialNumber: 'SN123',
//                 description: 'Updated elevator description',
//             }
//
//             const expectedElevator: CreatedElevatorDTO = {
//                 buildingId: dto.buildingId,
//                 identifier: dto.identifier,
//                 floors: dto.floors,
//                 brand: dto.brand,
//                 model: dto.model,
//                 serialNumber: dto.serialNumber,
//                 description: dto.description,
//             }
//
//             service.putElevator(dto).subscribe((patchedElevator) => {
//                 expect(patchedElevator).to.eq(expectedElevator)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
//             )
//             expect(req.request.method).to.eq('PUT')
//
//             req.flush(expectedElevator)
//         })
//
//         it('should handle an error when putting an elevator', () => {
//             const dto: EditElevatorDTO = {
//                 buildingId: 'B001',
//                 identifier: 1,
//                 floors: [1, 2, 3],
//                 brand: 'BrandX',
//                 model: 'ModelY',
//                 serialNumber: 'SN123',
//                 description: 'Updated elevator description',
//             }
//
//             const errorMessage = 'Error putting elevator'
//             const expectedError = new ErrorEvent(errorMessage)
//
//             service.putElevator(dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingId}/elevators/${dto.identifier}`,
//             )
//             expect(req.request.method).to.eq('')
//
//             req.error(expectedError)
//         })
//     })
// })
