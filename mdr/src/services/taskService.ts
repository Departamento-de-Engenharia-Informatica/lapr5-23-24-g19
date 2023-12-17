import { Inject, Service } from 'typedi'
import { Either, left, right } from '../core/logic/Result'

import ITaskService, { TaskErrorCode, TaskErrorResult } from './IServices/ITaskService'
import { ITaskTypeDTO } from '../dto/ITaskTypeDTO'
import { TaskType } from '../domain/robotType/taskType'
import config from '../../config'
import HttpNodeMdtAdapter from '../repos/mdt/httpNodeMdtAdapter'

@Service()
export default class TaskService implements ITaskService {
    constructor(@Inject(config.repos.mdt.name) private repo: HttpNodeMdtAdapter
     ){}
    async createTask(req:string): Promise<Either<TaskErrorResult, String>> {
        //TODO: parse to DTO

        try {
            
            const saved = await this.repo.create(req)
            return right(saved)

        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }

    async getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>> {
        try {
            const values = Object.values(TaskType).filter((value) => typeof value === 'string');

            const res = values.map((type: TaskType) => ({
                description: TaskType.toString(type),
            })) as ITaskTypeDTO[];

            return right(res)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }
}
