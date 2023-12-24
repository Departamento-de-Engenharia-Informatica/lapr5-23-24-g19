import { ITaskIdsDTO } from './ITaskIdsDTO'

export interface IRobotTasksDTO {
    Algorithm: string
    RobotTasks: { [robotName: string]: ITaskIdsDTO[] }
}
