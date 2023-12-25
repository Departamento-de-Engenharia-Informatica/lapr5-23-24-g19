// import { TestBed, inject } from '@angular/core/testing'
// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { RobotTypeRepo } from './RobotTypeRepo'
// import { Config } from '../config'
//
// describe('RobotTypeRepo: Unit Tests', () => {
//     let service: RobotTypeRepo
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//             providers: [RobotTypeRepo],
//         })
//
//         service = TestBed.inject(RobotTypeRepo)
//         httpMock = TestBed.inject(HttpTestingController)
//     })
//
//     afterEach(() => {
//         httpMock.verify()
//     })
//
//     describe('createRobotType()', () => {
//         it('should create a robot type successfully', () => {
//             const dto = {
//                 code: 'RT001',
//                 brand: 'Brand1',
//                 model: 'Model1',
//                 taskTypes: ['Task1', 'Task2'],
//             }
//
//             const expectedRobotType = { ...dto }
//
//             service.createRobotType(dto).subscribe((createdRobotType) => {
//                 expect(createdRobotType).to.eq(expectedRobotType)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robottypes`)
//             expect(req.request.method).to.eq('POST')
//
//             req.flush(expectedRobotType)
//         })
//
//         it('should return an error if creating a robot type fails', () => {
//             const dto = {
//                 code: 'RT001',
//                 brand: 'Brand1',
//                 model: 'Model1',
//                 taskTypes: ['Task1', 'Task2'],
//             }
//
//             service.createRobotType(dto).subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(422)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robottypes`)
//             expect(req.request.method).to.eq('POST')
//
//             req.error(new ErrorEvent('Error creating robot type'), { status: 422 })
//         })
//     })
//
//     describe('getRobotTypes()', () => {
//         it('should get a list of robot types successfully', () => {
//             const expectedRobotTypes = [
//                 {
//                     code: 'RT001',
//                     brand: 'Brand1',
//                     model: 'Model1',
//                     taskTypes: ['Task1', 'Task2'],
//                 },
//                 {
//                     code: 'RT002',
//                     brand: 'Brand2',
//                     model: 'Model2',
//                     taskTypes: ['Task3', 'Task4'],
//                 },
//             ]
//
//             service.getRobotTypes().subscribe((robotTypes) => {
//                 expect(robotTypes).to.eq(expectedRobotTypes)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robottypes`)
//             expect(req.request.method).to.eq('GET')
//
//             req.flush(expectedRobotTypes)
//         })
//
//         it('should return an error if robot types are not found', () => {
//             service.getRobotTypes().subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(404)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robottypes`)
//             expect(req.request.method).to.eq('GET')
//
//             req.error(new ErrorEvent('Robot types not found'), { status: 404 })
//         })
//     })
// })
