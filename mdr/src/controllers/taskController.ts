import config from '../../config'
import { Inject, Service } from 'typedi'
import ITaskController from './IControllers/ITaskController'
import ITaskService, {
    TaskErrorCode,
    TaskErrorResult,
} from '../services/IServices/ITaskService'
import { Request, Response, NextFunction, ParamsDictionary } from 'express-serve-static-core'
import { CreateSurveillanceTaskDTO } from '../../../spa/src/app/dto/CreateSurveillanceTaskDTO'
import { CreateDeliveryTaskDTO } from '../../../spa/src/app/dto/CreateDeliveryTaskDTO'
import { ParsedQs } from 'qs'
import { IFilterDTO } from '../dto/IFilterDTO'

@Service()
export default class TaskController implements ITaskController {
    constructor(@Inject(config.services.task.name) private service: ITaskService) {}
   

    async getTypes(_: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getTypes()

            if (result.isLeft()) {
                const err = result.value as TaskErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }
            return res.json(result.value).status(200)
        } catch (e) {
            return next(e)
        }
    }

    async createSurveillanceTask(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.createSurveillanceTask(
                req.body as CreateSurveillanceTaskDTO,
            )

            if (result.isLeft()) {
                const err = result.value as TaskErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            return res.json(result.value).status(200)
        } catch (e) {
            return next(e)
        }
    }

    async createDeliveryTask(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.createDeliveryTask(
                req.body as CreateDeliveryTaskDTO,
            )

            if (result.isLeft()) {
                const err = result.value as TaskErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            return res.json(result.value).status(200)
        } catch (e) {
            return next(e)
        }
    }
    async getByFilter(req: Request, res: Response, next: NextFunction) {
        try {
            var dto = {
                criteria: req.query.criteria.toString(),
                rule: req.query.rule.toString()
            }as IFilterDTO

            const result = await this.service.getByFilter(dto)

            if (result.isLeft()) {
                const err = result.value as TaskErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
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
