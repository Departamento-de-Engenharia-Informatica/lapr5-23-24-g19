// import { TestBed } from '@angular/core/testing'
// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { Config } from '../config'
// import { RoomService } from './room.service'
// import { RoomDTO } from '../dto/RoomDTO'
// import { CreatedRoomDTO } from '../dto/CreatedRoomDTO'
//
// describe('RoomService: Unit Tests', () => {
//     let service: RoomService
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//             providers: [RoomService],
//         })
//
//         service = TestBed.inject(RoomService)
//         httpMock = TestBed.inject(HttpTestingController)
//     })
//
//     afterEach(() => {
//         httpMock.verify()
//     })
//
//     describe('createRoom()', () => {
//         it('should create a room successfully', () => {
//             const buildingCode = 'B001'
//             const floorNumber = '1'
//             const dto: RoomDTO = {
//                 name: 'ktm',
//                 category: 'GABINETE',
//                 description: 'A new room',
//                 dimensions: { length: 10, width: 10 },
//                 positions: { x: 1, y: 2 },
//             }
//
//             const expectedRoom = {
//                 buildingCode,
//                 floorNumber,
//                 name: dto.name,
//                 category: dto.category,
//                 description: dto.description,
//                 dimensions: dto.dimensions,
//                 positions: dto.positions,
//             }
//
//             service
//                 .createRoom(buildingCode, floorNumber, dto)
//                 .subscribe((createdRoom) => {
//                     expect(createdRoom).to.eq(expectedRoom)
//                 })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
//             )
//             expect(req.request.method).to.eq('POST')
//
//             req.flush(expectedRoom)
//         })
//
//         it('should handle an error when creating an room', () => {
//             const buildingCode = 'B001'
//             const floorNumber = '1'
//             const dto: RoomDTO = {
//                 name: 'ktm',
//                 category: 'GABINETE',
//                 description: 'A new room',
//                 dimensions: { length: 10, width: 10 },
//                 positions: { x: 1, y: 2 },
//             }
//
//             const errorMessage = 'Error creating room'
//             const expectedError = new ErrorEvent(errorMessage)
//
//             service.createRoom(buildingCode, floorNumber, dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
//             )
//             expect(req.request.method).to.eq('POST')
//
//             req.error(expectedError)
//         })
//     })
//
//     describe('getRooms()', () => {
//         it('should get rooms successfully', () => {
//             const buildingCode = 'B001'
//             const floorNumber = 1
//             const dto: RoomDTO = {
//                 name: 'ktm',
//                 category: 'GABINETE',
//                 description: 'A new room',
//                 dimensions: { length: 10, width: 10 },
//                 positions: { x: 1, y: 2 },
//             }
//
//             const expectedRooms: CreatedRoomDTO[] = [
//                 {
//                     name: dto.name,
//                     buildingCode: buildingCode,
//                     floorNumber: floorNumber,
//                     category: dto.category,
//                     description: dto.description,
//                     dimensions: dto.dimensions,
//                     positions: dto.positions,
//                 },
//             ]
//
//             service.getRooms(buildingCode, floorNumber).subscribe((rooms) => {
//                 expect(rooms).to.eq(expectedRooms)
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
//             )
//             expect(req.request.method).to.eq('GET')
//
//             req.flush(expectedRooms)
//         })
//
//         it('should handle an error when getting rooms', () => {
//             const buildingCode = 'B001'
//             const floorNumber = 1
//             const errorMessage = 'Error getting rooms'
//             const expectedError = new ErrorEvent(errorMessage)
//
//             service.getRooms(buildingCode, floorNumber).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(
//                 `${Config.baseUrl}/buildings/${buildingCode}/floors/${floorNumber}/rooms`,
//             )
//             expect(req.request.method).to.eq('GET')
//
//             req.error(expectedError)
//         })
//     })
// })
