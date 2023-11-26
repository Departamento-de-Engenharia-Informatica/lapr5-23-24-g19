import config from '../../config'
import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'

import IFloorController from './IControllers/IFloorController'
import IFloorService from '../services/IServices/IFloorService'

import { IFloorDTO } from '../dto/IFloorDTO'
import { IBuildingCodeDTO } from '../dto/IBuildingCodeDTO'
import { IUpdateFloorDTO } from '../dto/IUpdateFloorDTO'
import { ErrorCode, ErrorResult } from '../services/IServices/IFloorService'

@Service()
export default class FloorController implements IFloorController {
    constructor(
        @Inject(config.services.floor.name) private floorServiceInstance: IFloorService,
    ) {}

    public async createFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingId = req.params.id

            const result = await this.floorServiceInstance.createFloor(
                req.body as IFloorDTO,
                buildingId,
            )

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IFloorDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    public async patchFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IUpdateFloorDTO
            dto.oldFloorNumber = parseInt(req.params.floor)
            dto.buildingCode = req.params.id

            const result = await this.floorServiceInstance.patchFloor(
                req.body as IUpdateFloorDTO,
            )

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IUpdateFloorDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async putFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IUpdateFloorDTO
            dto.oldFloorNumber = parseInt(req.params.floor)
            dto.buildingCode = req.params.id

            const result = await this.floorServiceInstance.putFloor(
                req.body as IUpdateFloorDTO,
            )

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IUpdateFloorDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getFloors(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.floorServiceInstance.getFloors(req.params.id)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IFloorDTO[]
            return res.json(message).status(200)
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
