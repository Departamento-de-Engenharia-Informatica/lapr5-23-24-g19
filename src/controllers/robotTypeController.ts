import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IRobotTypeController from './IControllers/IRobotTypeController'
import { IRobotTypeDTO } from '../dto/IRobotTypeDTO'
import IRobotTypeService from '../services/IServices/IRobotTypeService'

@Service()
export default class RobotTypeController implements IRobotTypeController {
    constructor(@Inject(config.services.robotType.name) private service: IRobotTypeService) {}

    public async createRobotType(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IRobotTypeDTO

            const result = await this.service.createRobotType(dto)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(201)
        } catch (e) {
            return next(e)
        }
    }
}
