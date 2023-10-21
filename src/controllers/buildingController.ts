import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IBuildingController from './IControllers/IBuildingController'
import IBuildingService from '../services/IServices/IBuildingService'
import { IBuildingDTO } from '../dto/IBuildingDTO'

import { Result } from '../core/logic/Result'

@Service()
export default class BuildingController implements IBuildingController {
    constructor(@Inject(config.services.building.name) private service: IBuildingService) {}

    public async createBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingOrError = (await this.service.createBuilding(
                req.body as IBuildingDTO,
            )) as Result<IBuildingDTO>

            if (buildingOrError.isFailure) {
                return res.status(422).send()
            }

            const buildingDTO = buildingOrError.getValue()
            return res.json(buildingDTO).status(201)
        } catch (e) {
            return next(e)
        }
    }
}
