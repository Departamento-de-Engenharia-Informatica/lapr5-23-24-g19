import { Either } from '../../core/logic/Result'
import { ICreatedRobotDTO } from '../../dto/ICreatedRobotDTO'
import { IRobotDTO } from '../../dto/IRobotDTO'
import { IRobotInhibitDTO } from '../../dto/IRobotInhibitDTO'
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
    createTask(String): Promise<Either<TaskErrorResult, String>>
    getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>>
}
