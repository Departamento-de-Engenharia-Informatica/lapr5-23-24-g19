import { CreateDeliveryTaskDTO } from '../../../../spa/src/app/dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../../../../spa/src/app/dto/CreateSurveillanceTaskDTO'
import { Either } from '../../core/logic/Result'
import { IFilterDTO } from '../../dto/IFilterDTO'
import { ITaskIdsDTO } from '../../dto/ITaskIdsDTO'
import { ITaskTypeDTO } from '../../dto/ITaskTypeDTO'
import { IUpdateTaskDTO } from '../../dto/IUpdateTaskDTO'

export enum TaskErrorCode {
    NotFound,
    BussinessRuleViolation,
}

export type TaskErrorResult = {
    errorCode: TaskErrorCode
    message: string
}

export default interface ITaskService {
    getByFilter(DTO: IFilterDTO): Promise<Either<TaskErrorResult, String>>
    getByStatus(status: string): Promise<Either<TaskErrorResult, String>>
    createSurveillanceTask(
        dto: CreateSurveillanceTaskDTO,
    ): Promise<Either<TaskErrorResult, String>>
    createDeliveryTask(
        dto: CreateDeliveryTaskDTO,
    ): Promise<Either<TaskErrorResult, String>>
    updateTask(DTO: IUpdateTaskDTO): Promise<Either<TaskErrorResult, String>>
    getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>>

    taskSequence(dto: ITaskIdsDTO): Promise<Either<TaskErrorResult, String>>
}
