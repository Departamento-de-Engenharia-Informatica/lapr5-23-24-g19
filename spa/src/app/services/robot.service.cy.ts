// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { TestBed } from '@angular/core/testing'
// import { Config } from '../config'
// import { RobotService } from './robot.service'
//
// describe('RobotService', () => {
//     let service: RobotService
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//             providers: [RobotService],
//         })
//
//         service = TestBed.inject(RobotService)
//         httpMock = TestBed.inject(HttpTestingController)
//     })
//
//     afterEach(() => {
//         httpMock.verify()
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
//     })
//
//     describe('getRobotsOnion()', () => {
//         it('should get a list of robots using RobotRepo successfully', () => {
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
//             service.getRobotsOnion().subscribe((robots) => {
//                 expect(robots).to.eq(expectedRobots)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots`)
//             expect(req.request.method).to.eq('GET')
//
//             req.flush(expectedRobots)
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
//         it('should handle an error if creating a robot fails', () => {
//             const dto = {
//                 code: 'R001',
//                 nickname: 'Robo1',
//                 typeCode: 'T001',
//                 serialNumber: 'SN001',
//                 description: 'A cool robot',
//             }
//
//             const errorMessage = 'Error creating robot'
//
//             service.createRobot(dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots`)
//             expect(req.request.method).to.eq('POST')
//
//             req.error(new ErrorEvent(errorMessage), {
//                 status: 422,
//                 statusText: 'Error creating robot',
//             })
//         })
//     })
//
//     describe('inhibit()', () => {
//         it('should inhibit a robot successfully', () => {
//             const dto = {
//                 code: 'R001',
//                 state: 1,
//             }
//
//             const expectedRobot = {
//                 code: 'R001',
//                 nickname: 'Robo1',
//                 typeCode: 'T001',
//                 serialNumber: 'SN001',
//                 description: 'A cool robot',
//                 state: 1,
//             }
//
//             service.inhibit(dto).subscribe((inhibitedRobot) => {
//                 expect(inhibitedRobot).to.eq(expectedRobot)
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots/${dto.code}/inhibit`)
//             expect(req.request.method).to.eq('PATCH')
//
//             req.flush(expectedRobot)
//         })
//         it('should handle an error if inhibiting a robot fails', () => {
//             const dto = {
//                 code: 'R001',
//                 state: 1,
//             }
//
//             const errorMessage = 'Error inhibiting robot'
//
//             service.inhibit(dto).subscribe({
//                 error: (error) => {
//                     expect(error.message).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robots/${dto.code}/inhibit`)
//             expect(req.request.method).to.eq('PATCH')
//
//             req.error(new ErrorEvent(errorMessage), {
//                 status: 422,
//                 statusText: 'Error inhibiting robot',
//             })
//         })
//     })
// })
