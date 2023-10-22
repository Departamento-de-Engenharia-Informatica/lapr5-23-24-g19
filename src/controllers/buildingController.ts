import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IBuildingController from './IControllers/IBuildingController'
import IBuildingService from '../services/IServices/IBuildingService'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { BuildingCode } from '../domain/building/buildingCode'

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

    public async editBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IBuildingDTO;
            dto.code = req.params.id;
            const result = await this.service.createBuilding(dto)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(201);
        } catch (e) {
            return next(e)
        }
    }
    
    public async patchBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.editBuilding(req.params.id, req.body as IBuildingDTO)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(201);
        } catch (e) {
            return next(e)
        }
    }

    public async getBuildings(req: Request, res: Response, next: NextFunction) {
        try {

            const result = await this.service.getBuildings()
            if (result.isFailure) {
                return res.status(422).send()
            }
            return res.json(result.getValue()).status(200);
        } catch (e) {
            return next(e)
        }
    }
    

}
