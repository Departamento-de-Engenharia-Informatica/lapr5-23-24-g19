// import {
//     HttpClientTestingModule,
//     HttpTestingController,
// } from '@angular/common/http/testing'
// import { TestBed } from '@angular/core/testing'
// import { Config } from '../config'
// import { CreateRobotTypeDTO } from '../dto/CreateRobotTypeDTO'
// import { RobotTypeService } from './robot-type.service'
//
// describe('RobotTypeService', () => {
//     let service: RobotTypeService
//     let httpMock: HttpTestingController
//
//     beforeEach(() => {
//         TestBed.configureTestingModule({
//             imports: [HttpClientTestingModule],
//             providers: [RobotTypeService],
//         })
//
//         service = TestBed.inject(RobotTypeService)
//         httpMock = TestBed.inject(HttpTestingController)
//     })
//
//     afterEach(() => {
//         httpMock.verify()
//     })
//
//     describe('createRobotType()', () => {
//         it('should create a robot type successfully', () => {
//             const dto: CreateRobotTypeDTO = {
//                 code: 'RT001',
//                 brand: 'Brand1',
//                 model: 'Model1',
//                 taskTypes: ['Task1', 'Task2'],
//             }
//
//             service.createRobotType(dto).subscribe((result) => {
//                 expect(result).to.eq('Robot created successfully!')
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robottypes`)
//             expect(req.request.method).to.eq('POST')
//
//             req.flush({})
//         })
//
//         it('should handle an error if creating a robot type fails', () => {
//             const dto: CreateRobotTypeDTO = {
//                 code: 'RT001',
//                 brand: 'Brand1',
//                 model: 'Model1',
//                 taskTypes: ['Task1', 'Task2'],
//             }
//
//             const errorMessage = 'Error creating robot type'
//
//             service.createRobotType(dto).subscribe({
//                 error: (error) => {
//                     expect(error).to.eq(errorMessage)
//                 },
//             })
//
//             const req = httpMock.expectOne(`${Config.baseUrl}/robottypes`)
//             expect(req.request.method).to.eq('POST')
//
//             req.error(new ErrorEvent(errorMessage), {
//                 status: 422,
//                 statusText: 'Error creating robot type',
//             })
//         })
//     })
// })
