import { CreateDeliveryTaskDTO } from '../../../../spa/src/app/dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../../../../spa/src/app/dto/CreateSurveillanceTaskDTO'
import { Either } from '../../core/logic/Result'
import { IFilterDTO } from '../../dto/IFilterDTO'
import { ITaskTypeDTO } from '../../dto/ITaskTypeDTO'

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
    createSurveillanceTask(
        dto: CreateSurveillanceTaskDTO,
    ): Promise<Either<TaskErrorResult, String>>
    createDeliveryTask(
        dto: CreateDeliveryTaskDTO,
    ): Promise<Either<TaskErrorResult, String>>
    getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>>
}
