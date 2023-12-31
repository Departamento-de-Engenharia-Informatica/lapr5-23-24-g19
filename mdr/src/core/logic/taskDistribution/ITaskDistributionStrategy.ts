import Robot from '../../../domain/robot/Robot'
import { ITaskIdsDTO } from '../../../dto/ITaskIdsDTO'
import { Result } from '../Result'

export interface AssignedTasks {
    [robotName: string]: ITaskIdsDTO[]
}

export default interface ITaskDistributionStrategy {
    distribute(tasks: ITaskIdsDTO[], robots: Robot[]): Result<AssignedTasks>
}
