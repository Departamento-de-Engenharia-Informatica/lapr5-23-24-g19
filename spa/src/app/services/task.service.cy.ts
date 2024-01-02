import { TestBed } from '@angular/core/testing'
import { TaskService } from './task.service'
import {
    HttpClientTestingModule,
    HttpTestingController,
} from '@angular/common/http/testing'
import { UpdateTaskDTO } from '../dto/UpdateTaskDTO'
import { TaskDTO, TaskState } from '../dto/TaskDTO'
import { Config } from '../config'
import { AuthService } from "@auth0/auth0-angular";
import { of } from "rxjs";

describe('Task Service Unit Testing', () => {
    let service: TaskService
    let httpMock: HttpTestingController
    const authMock = {
        getAccessTokenSilently: () => of('mock-token'), // Returns a mock token
    };

    beforeEach(() => {
        TestBed.configureTestingModule({
            providers: [TaskService, { provide: AuthService, useValue: authMock }],
            imports: [HttpClientTestingModule],
        })

        service = TestBed.inject(TaskService)
        httpMock = TestBed.inject(HttpTestingController)
    })

    afterEach(() => httpMock.verify())

    it('should be created', () => {
        expect(service).to.exist
    })

    describe('updateTask()', () => {
        it("should be able to update a task's approval state", () => {
            const dto: UpdateTaskDTO = {
                id: 'ffc15596-4322-4a67-b4f8-880140a23232',
                taskStatus: 'Approved',
            }

            const expected: Partial<TaskDTO> = {
                id: 'ffc15596-4322-4a67-b4f8-880140a23232',
                state: TaskState.APPROVED,
            }

            service.updateTask(dto).subscribe({
                next: (updatedTask) => {
                    expect(updatedTask.id).to.equal(expected.id)
                    expect(updatedTask.state).to.equal(expected.state)
                },
                error: () => assert.fail(),
            })

            const req = httpMock.expectOne(`${Config.baseUrl}/task/${dto.id}`)
            expect(req.request.method).to.equal('PATCH')

            req.flush(expected)
        })

        it("should error when updating a tak's approval state fails", () => {
            const dto: UpdateTaskDTO = {
                id: 'ffc15596-4322-4a67-b4f8-880140a23232',
                taskStatus: 'BadState',
            }

            const errmsg = 'Task not updated'

            service.updateTask(dto).subscribe({
                next: () => assert.fail(),
                error: (err) => {
                    expect(err.message).to.equal(errmsg)
                },
            })

            const req = httpMock.expectOne(`${Config.baseUrl}/task/${dto.id}`)
            expect(req.request.method).to.equal('PATCH')

            req.error(new ErrorEvent(errmsg), { status: 422 })
        })
    })
})
