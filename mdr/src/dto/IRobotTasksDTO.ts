import { ITaskIdsDTO } from './ITaskIdsDTO'

export interface IRobotTasksDTO {
    RobotTasks: { [robotName: string]: ITaskIdsDTO[] }
}
