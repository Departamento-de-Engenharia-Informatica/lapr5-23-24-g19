import config from '../../config'
import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'

import IFloorMapController from './IControllers/IFloorMapController'
import IFloorMapService from '../services/IServices/IFloorMapService'

import { IBuildingFloorDTO } from '../dto/IBuildingFloorDTO'
import { IFloorMapDTO } from '../dto/IFloorMapDTO'
import { ErrorCode, ErrorResult } from '../services/IServices/IFloorService'

@Service()
export default class FloorMapController implements IFloorMapController {
    constructor(
        @Inject(config.services.floorMap.name) private service: IFloorMapService,
    ) {}

    public async updateMap(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IFloorMapDTO
            dto.buildingCode = req.params.id
            dto.floorNumber = parseInt(req.params.floorNumber)

            const result = await this.service.uploadMap(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret = this.resolveHttpCode(error.errorCode)

                return res.status(ret).send(error.message)
            }

            const message = result.value as IFloorMapDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getMap(req: Request, res: Response, next: NextFunction) {
        try {
            const dto: IBuildingFloorDTO = {
                buildingCode: req.params.id,
                floorNumber: parseInt(req.params.floorNumber),
            }
            const result = await this.service.getMap(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret = this.resolveHttpCode(error.errorCode)

                return res.status(ret).send(error.message)
            }

            const message = result.value as IFloorMapDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: ErrorCode) {
        let ret: number
        switch (result) {
            case ErrorCode.BusinessRuleViolation:
                ret = 422
                break
            case ErrorCode.NotFound:
                ret = 404
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
