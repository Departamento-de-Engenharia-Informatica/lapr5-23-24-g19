import { CreateDeliveryTaskDTO } from '../../dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../../dto/CreateSurveillanceTaskDTO'
import { Either } from '../../core/logic/Result'
import { IFilterDTO } from '../../dto/IFilterDTO'
import { IGeneralTaskDTO } from '../../dto/IGeneralTaskDTO'
import { IRobotTaskSequenceDTO } from '../../dto/IRobotTaskSequenceDTO'
import { ISequenceAlgorithmDTO } from '../../dto/ISequenceAlgorithmDTO'
import { ITaskAlgorithmDTO } from '../../dto/ITaskAlgorithmDTO'
import { ITaskTypeDTO } from '../../dto/ITaskTypeDTO'
import { IUpdateTaskDTO } from '../../dto/IUpdateTaskDTO'
import { IUpdatedTaskDTO } from '../../dto/IUpdatedTaskDTO'

export enum TaskErrorCode {
    NotFound,
    BussinessRuleViolation,
    AdapterFailure,
}

export type TaskErrorResult = {
    errorCode: TaskErrorCode
    message: string
}

export default interface ITaskService {
    getByFilter(DTO: IFilterDTO): Promise<Either<TaskErrorResult, String>>
    getByStatus(status: string): Promise<Either<TaskErrorResult, IGeneralTaskDTO[]>>
    createSurveillanceTask(
        dto: CreateSurveillanceTaskDTO,
    ): Promise<Either<TaskErrorResult, String>>
    createDeliveryTask(
        dto: CreateDeliveryTaskDTO,
    ): Promise<Either<TaskErrorResult, String>>
    updateTask(DTO: IUpdateTaskDTO): Promise<Either<TaskErrorResult, IUpdatedTaskDTO>>
    getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>>

    taskSequence(
        dto: ITaskAlgorithmDTO,
    ): Promise<Either<TaskErrorResult, IRobotTaskSequenceDTO>>
    taskSequenceAlgorithms(): Promise<Either<TaskErrorResult, ISequenceAlgorithmDTO[]>>
}
