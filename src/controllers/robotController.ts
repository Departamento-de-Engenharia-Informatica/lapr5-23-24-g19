import config from '../../config'
import { Inject, Service } from 'typedi'
import { Request, Response, NextFunction } from 'express'

import IRobotController from './IControllers/IRobotController'
import IRobotService from '../services/IServices/IRobotService'
import { IRobotDTO } from '../dto/IRobotDTO'
import { IRobotInhibitDTO } from '../dto/IRobotInhibitDTO'

@Service()
export default class RobotController implements IRobotController {
    constructor(@Inject(config.services.robot.name) private service: IRobotService) {}

    async createRobot(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IRobotDTO
            const result = await this.service.createRobot(dto)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(201)
        } catch (e) {
            return next(e)
        }
    }

    async getRobots(_: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getRobots()

            if (result.isFailure) {
                // TODO: 404 code instead?
                return res.status(412).send()
            }

            return res.json(result.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }

    async inhibitRobot(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = { code: req.params.id } as IRobotInhibitDTO
            const result = await this.service.inhibitRobot(dto)

            if (result.isFailure) {
                return res.status(404).send()
            }

            // TODO: double check return code
            return res.json(result.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }
}
