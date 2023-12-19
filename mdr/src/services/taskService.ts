import { Service } from 'typedi'
import { Either, left, right } from '../core/logic/Result'

import ITaskService, { TaskErrorCode, TaskErrorResult } from './IServices/ITaskService'
import { ITaskTypeDTO } from '../dto/ITaskTypeDTO'
import { TaskType } from '../domain/robotType/taskType'

@Service()
export default class TaskService implements ITaskService {
    constructor() {}

    async getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>> {
        try {
            const values = Object.values(TaskType).filter(
                (value) => typeof value === 'string',
            )

            const res = values.map((type: TaskType) => ({
                description: TaskType.toString(type),
            })) as ITaskTypeDTO[]

            return right(res)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }
}
