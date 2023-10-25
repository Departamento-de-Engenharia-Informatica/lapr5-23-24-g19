import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IFloorController from './IControllers/IFloorController'
import IFloorService from '../services/IServices/IFloorService'

import { Result } from '../core/logic/Result'
import { IFloorDTO } from '../dto/IFloorDTO'
import { IFloorMapDTO } from '../dto/IFloorMapDTO'
import { ParamsDictionary } from 'express-serve-static-core'
import { ParsedQs } from 'qs'
import { IBuildingCodeDTO } from '../dto/IBuildingCodeDTO'

@Service()
export default class FloorController implements IFloorController {
    constructor(@Inject(config.services.floor.name) private floorServiceInstance: IFloorService) {}

    public async createFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingId = req.params.id
            const roleOrError = (await this.floorServiceInstance.createFloor(
                req.body as IFloorDTO,
                buildingId,
            )) as Result<IFloorDTO>

            if (roleOrError.isFailure) {
                return res.status(402).send()
            }

            const floorDTO = roleOrError.getValue()
            return res.json(floorDTO).status(201)
        } catch (e) {
            return next(e)
        }
    }

    public async getFloors(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.floorServiceInstance.getFloors(req.params.id)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async updateMap(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IFloorMapDTO
            dto.buildingCode = req.params.id
            dto.floorNumber = parseInt(req.params.floorNumber)

            const mapOrError = (await this.floorServiceInstance.uploadMap(dto)) as Result<IFloorMapDTO>

            if (mapOrError.isFailure) {
                return res.status(402).send()
            }

            const floorDTO = mapOrError.getValue()
            return res.json(floorDTO).status(201)
        } catch (e) {
            return next(e)
        }
    }

    async floorsWithPassage(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = { code: req.params.id } as IBuildingCodeDTO

            const floors = await this.floorServiceInstance.floorsWithPassage(dto)

            if (floors.isFailure) {
                return res.status(404).send()
            }

            return res.json(floors.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }
}
