import config from '../../config'
import { Inject, Service } from 'typedi'
import { Request, Response, NextFunction } from 'express'

import IRobotController from './IControllers/IRobotController'
import IRobotService, { RobotErrorCode, RobotErrorResult } from '../services/IServices/IRobotService'
import { IRobotDTO } from '../dto/IRobotDTO'
import { IRobotInhibitDTO } from '../dto/IRobotInhibitDTO'

@Service()
export default class RobotController implements IRobotController {
    constructor(@Inject(config.services.robot.name) private service: IRobotService) {}

    async createRobot(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IRobotDTO
            const result = await this.service.createRobot(dto)

            if (result.isLeft()) {
                const err = result.value as RobotErrorResult
                return res.status(this.resolveHttpCode(err.errorCode)).send(JSON.stringify(err.message))
            }

            return res.json(result.value).status(201)
        } catch (e) {
            return next(e)
        }
    }

    async getRobots(_: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getRobots()

            if (result.isLeft()) {
                const err = result.value as RobotErrorResult
                return res.status(this.resolveHttpCode(err.errorCode)).send(JSON.stringify(err.message))
            }

            return res.json(result.value).status(200)
        } catch (e) {
            return next(e)
        }
    }

    async inhibitRobot(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = { code: req.params.id } as IRobotInhibitDTO
            const result = await this.service.inhibitRobot(dto)

            if (result.isLeft()) {
                const err = result.value as RobotErrorResult
                return res.status(this.resolveHttpCode(err.errorCode)).send(JSON.stringify(err.message))
            }

            return res.json(result.value).status(200)
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: RobotErrorCode) {
        let ret: number
        switch (result) {
            case RobotErrorCode.BussinessRuleViolation:
                ret = 422
                break
            case RobotErrorCode.NotFound:
                ret = 404
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
