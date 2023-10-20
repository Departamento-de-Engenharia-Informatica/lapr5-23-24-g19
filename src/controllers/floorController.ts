import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IFloorController from './IControllers/IFloorController'
import IFloorService from '../services/IServices/IFloorService'

import { Result } from '../core/logic/Result'

@Service()
export default class FloorController implements IFloorController {
    constructor(@Inject(config.services.floor.name) private floorServiceInstance: IFloorService) {}

    public async createFloor(req: Request, res: Response, next: NextFunction) {
        /*try {
            const buildingOrError = (await this.floorServiceInstance.createBuilding(
                req.body as IBuildingDTO,
            )) as Result<IBuildingDTO>

            if (buildingOrError.isFailure) {
                return res.status(402).send()
            }

            const buildingDTO = buildingOrError.getValue()
            return res.json(buildingDTO).status(201)
        } catch (e) {
            return next(e)
        }
        */
    }
}
