import config from '../../config'
import { Inject, Service } from 'typedi'
import { Request, Response, NextFunction } from 'express'

import IRobotController from './IControllers/IRobotController'
import IRobotService from '../services/IServices/IRobotService'
import { IRobotDTO } from '../dto/IRobotDTO'

@Service()
export default class RobotController implements IRobotController {
    constructor(@Inject(config.services.robot.name) private service: IRobotService) {}

    public async createRobot(req: Request, res: Response, next: NextFunction) {
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
}
