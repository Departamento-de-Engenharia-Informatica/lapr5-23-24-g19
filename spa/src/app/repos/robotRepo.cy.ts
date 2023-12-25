// import { TestBed, inject } from '@angular/core/testing'
// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { RobotRepo } from './RobotRepo'
// import { Config } from '../config'
//
// describe('RobotRepo', () => {
//     let service: RobotRepo
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//             providers: [RobotRepo],
//         })
//
//         service = TestBed.inject(RobotRepo)
//         httpMock = TestBed.inject(HttpTestingController)
//     })
//
//     afterEach(() => {
//         httpMock.verify()
//     })
//
//     describe('createRobot()', () => {
//         it('should create a robot successfully', () => {
//             const dto = {
//                 code: 'R001',
//                 nickname: 'Robo1',
//                 typeCode: 'T001',
//                 serialNumber: 'SN001',
//                 description: 'A cool robot',
//             }
//
//             const expectedRobot = {
//                 code: dto.code,
//                 nickname: dto.nickname,
//                 typeCode: dto.typeCode,
//                 serialNumber: dto.serialNumber,
//                 description: dto.description,
//                 state: 0,
//             }
//
//             service.createRobot(dto).subscribe((createdRobot) => {
//                 expect(createdRobot).to.eq(expectedRobot)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots`)
//             expect(req.request.method).to.eq('POST')
//
//             req.flush(expectedRobot)
//         })
//
//         it('should return an error if creating a robot fails', () => {
//             const dto = {
//                 code: 'R001',
//                 nickname: 'Robo1',
//                 typeCode: 'T001',
//                 serialNumber: 'SN001',
//                 description: 'A cool robot',
//             }
//
//             service.createRobot(dto).subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(422)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots`)
//             expect(req.request.method).to.eq('POST')
//
//             req.error(new ErrorEvent('Error creating robot'), { status: 422 })
//         })
//     })
//
//     describe('getRobots()', () => {
//         it('should get a list of robots successfully', () => {
//             const expectedRobots = [
//                 {
//                     code: 'R001',
//                     nickname: 'Robo1',
//                     typeCode: 'T001',
//                     serialNumber: 'SN001',
//                     description: 'A cool robot',
//                     state: 0,
//                 },
//                 {
//                     code: 'R002',
//                     nickname: 'Robo2',
//                     typeCode: 'T002',
//                     serialNumber: 'SN002',
//                     description: 'Another cool robot',
//                     state: 0,
//                 },
//             ]
//
//             service.getRobots().subscribe((robots) => {
//                 expect(robots).to.eq(expectedRobots)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots`)
//             expect(req.request.method).to.eq('GET')
//
//             req.flush(expectedRobots)
//         })
//
//         it('should return an error if creating a robot fails', () => {
//             service.getRobots().subscribe({
//                 error: (error) => {
//                     expect(error.status).to.eq(404)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots`)
//             expect(req.request.method).to.eq('GET')
//
//             req.error(new ErrorEvent('Robots not found'), { status: 404 })
//         })
//     })
// })
