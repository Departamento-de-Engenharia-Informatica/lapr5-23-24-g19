import config from '../../config'
import { Inject, Service } from 'typedi'
import { Request, Response, NextFunction } from 'express'

import { RobotErrorCode, RobotErrorResult } from '../services/IServices/IRobotService'
import ITaskController from './IControllers/ITaskController'
import ITaskService, { TaskErrorCode, TaskErrorResult } from '../services/IServices/ITaskService'

@Service()
export default class TaskController implements ITaskController {
    constructor(@Inject(config.services.task.name) private service: ITaskService) {}

    async getTypes(_: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getTypes()

            if (result.isLeft()) {
                const err = result.value as TaskErrorResult
                return res.status(this.resolveHttpCode(err.errorCode)).send(JSON.stringify(err.message))
            }

            return res.json(result.value).status(200)
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: TaskErrorCode) {
        let ret: number
        switch (result) {
            case TaskErrorCode.BussinessRuleViolation:
                ret = 422
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
