import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IFloorController from './IControllers/IFloorController'
import IFloorService from '../services/IServices/IFloorService'
import IBuildingService from '../services/IServices/IBuildingService'

import { Result } from '../core/logic/Result'
import { IFloorDTO } from '../dto/IFloorDTO'

@Service()
export default class FloorController implements IFloorController {
    constructor(@Inject(config.services.floor.name) private floorServiceInstance: IFloorService,@Inject(config.services.building.name) private buildingServiceInstance: IBuildingService) {}

    public async createFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingId= req.params.id;
            const roleOrError = (await this.floorServiceInstance.createFloor(req.body as IFloorDTO, buildingId)) as Result<IFloorDTO>

            if (roleOrError.isFailure) {
                return res.status(402).send()
            }

            const floorDTO = roleOrError.getValue()
            return res.json(floorDTO).status(201)
        } catch (e) {
            return next(e)
        }
    }
}
