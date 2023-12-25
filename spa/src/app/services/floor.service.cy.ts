// import { HttpClient } from '@angular/common/http'
// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { TestBed } from '@angular/core/testing'
// import { Config } from '../config'
// import {
//     FloorAndBuildingDTO,
//     FloorService,
//     PatchFloorDTO,
//     PutFloorDTO,
// } from './floor.service'
//
// describe('FloorService: Unit Testing', () => {
//     let service: FloorService
//     let httpClient: HttpClient
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//         })
//
//         service = TestBed.inject(FloorService)
//         httpClient = TestBed.inject(HttpClient)
//         httpMock = TestBed.inject(HttpTestingController)
//     })
//
//     afterEach(() => {
//         httpMock.verify()
//     })
//
//     it('should be created', () => {
//         expect(service).to.exist
//     })
//
//     describe('getFloors()', () => {
//         it('should be able to get floors if they exist', () => {
//             const buildingCode = 'A'
//             const mockFloors = [
//                 { floorNumber: 1, name: 'Floor 1' },
//                 { floorNumber: 2, name: 'Floor 2' },
//             ]
//
//             service.getFloors(buildingCode).subscribe((floors) => {
//                 expect(floors).eq(mockFloors)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/floors`,
//             )
//
//             expect(req.request.method).to.eq('GET')
//             req.flush(mockFloors)
//         })
//
//         it('should return an error if floors do not exist', () => {
//             const buildingCode = 'B'
//
//             service.getFloors(buildingCode).subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(404)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/floors`,
//             )
//             expect(req.request.method).to.eq('GET')
//
//             req.error(new ErrorEvent('Floors not found'), { status: 404 })
//         })
//     })
//
//     describe('createFloor()', () => {
//         it('should be able to create a floor successfully', () => {
//             const dto: FloorAndBuildingDTO = {
//                 buildingCode: 'A',
//                 floorNumber: 3,
//                 description: 'Floor 3',
//             }
//
//             service.createFloor(dto).subscribe((createdFloor) => {
//                 expect(createdFloor).to.eq(dto)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingCode}/floors`,
//             )
//
//             expect(req.request.method).to.eq('POST')
//             req.flush(dto)
//         })
//
//         it('should return an error if floor already exists', () => {
//             const dto: FloorAndBuildingDTO = {
//                 buildingCode: 'B',
//                 floorNumber: 1,
//                 description: 'Floor 1',
//             }
//
//             service.createFloor(dto).subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(422)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingCode}/floors`,
//             )
//             expect(req.request.method).to.eq('POST')
//
//             req.error(new ErrorEvent('Floor already exists'), { status: 422 })
//         })
//     })
//
//     describe('patchFloor()', () => {
//         it('should be able to patch a floor successfully', () => {
//             const dto: PatchFloorDTO = {
//                 buildingCode: 'A',
//                 oldFloorNumber: 2,
//                 newFloorNumber: 3,
//                 newDescription: 'Updated Floor 3',
//             }
//
//             service.patchFloor(dto).subscribe((patchedFloor) => {
//                 expect(patchedFloor).to.eq({
//                     buildingCode: 'A',
//                     floorNumber: 3,
//                     description: 'Updated Floor 3',
//                 })
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
//             )
//
//             expect(req.request.method).to.eq('PATCH')
//             req.flush({
//                 buildingCode: 'A',
//                 floorNumber: 3,
//                 description: 'Updated Floor 3',
//             })
//         })
//
//         it('should return an error if floor patch fails', () => {
//             const dto: PatchFloorDTO = {
//                 buildingCode: 'B',
//                 oldFloorNumber: 1,
//                 newFloorNumber: 2,
//                 newDescription: 'Updated Floor 2',
//             }
//
//             service.patchFloor(dto).subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(422)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
//             )
//             expect(req.request.method).to.eq('PATCH')
//
//             req.error(new ErrorEvent('Error while patching floor'), { status: 422 })
//         })
//     })
//
//     describe('putFloor()', () => {
//         it('should be able to put a floor successfully', () => {
//             const dto: PutFloorDTO = {
//                 buildingCode: 'A',
//                 oldFloorNumber: 2,
//                 newFloorNumber: 3,
//                 newDescription: 'Updated Floor 3',
//             }
//
//             service.putFloor(dto).subscribe((putFloor) => {
//                 expect(putFloor).to.eq({
//                     buildingCode: 'A',
//                     floorNumber: 3,
//                     description: 'Updated Floor 3',
//                 })
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
//             )
//             expect(req.request.method).to.eq('PUT')
//             req.flush({
//                 buildingCode: 'A',
//                 floorNumber: 3,
//                 description: 'Updated Floor 3',
//             })
//         })
//
//         it('should return an error if floor put fails', () => {
//             const dto: PutFloorDTO = {
//                 buildingCode: 'B',
//                 oldFloorNumber: 1,
//                 newFloorNumber: 2,
//                 newDescription: 'Updated Floor 2',
//             }
//
//             service.putFloor(dto).subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(422)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${dto.buildingCode}/floors/${dto.oldFloorNumber}`,
//             )
//             expect(req.request.method).to.eq('PUT')
//
//             req.error(new ErrorEvent('Error while putting floor'), { status: 422 })
//         })
//     })
// })
