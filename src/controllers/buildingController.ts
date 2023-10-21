import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IBuildingController from './IControllers/IBuildingController'
import IBuildingService from '../services/IServices/IBuildingService'
import { IBuildingDTO } from '../dto/IBuildingDTO'

@Service()
export default class BuildingController implements IBuildingController {
    constructor(@Inject(config.services.building.name) private service: IBuildingService) {}

    public async createBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.createBuilding(req.body as IBuildingDTO)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(201);
        } catch (e) {
            return next(e)
        }
    }
}
