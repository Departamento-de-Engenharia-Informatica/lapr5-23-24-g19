import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IRobotTypeController from './IControllers/IRobotTypeController'
import { IRobotTypeDTO } from '../dto/IRobotTypeDTO'
import IRobotTypeService, {
    RobotTypeErrorCode,
    RobotTypeErrorResult,
} from '../services/IServices/IRobotTypeService'
import { IRobotDTO } from '../dto/IRobotDTO'

@Service()
export default class RobotTypeController implements IRobotTypeController {
    constructor(
        @Inject(config.services.robotType.name) private service: IRobotTypeService,
    ) {}

    async createRobotType(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IRobotTypeDTO

            const result = await this.service.createRobotType(dto)

            if (result.isLeft()) {
                const err = result.value as RobotTypeErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as IRobotTypeDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async getRobotTypes(_: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getRobotTypes()

            if (result.isLeft()) {
                const err = result.value as RobotTypeErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as IRobotTypeDTO[]
            return res.status(200).send(message)
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: RobotTypeErrorCode) {
        let ret: number
        switch (result) {
            case RobotTypeErrorCode.BussinessRuleViolation:
                ret = 422
                break
            case RobotTypeErrorCode.NotFound:
                ret = 404
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
