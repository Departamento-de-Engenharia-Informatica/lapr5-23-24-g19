import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IBuildingController from './IControllers/IBuildingController'
import IBuildingService, {
    ErrorResult,
    ErrorCode,
} from '../services/IServices/IBuildingService'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { IBuildingEditDTO } from '../dto/IBuildingEditDTO'
import { IBuildingMinMaxFloorsDTO } from '../dto/IBuildingMinMaxFloorsDTO'

@Service()
export default class BuildingController implements IBuildingController {
    constructor(
        @Inject(config.services.building.name) private service: IBuildingService,
    ) {}

    public async createBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.createBuilding(req.body as IBuildingDTO)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }
            const message = result.value as IBuildingDTO
            return res.status(201).send(JSON.stringify(message))
        } catch (e) {
            return next(e)
        }
    }

    public async putBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IBuildingDTO
            dto.code = req.params.id
            const result = await this.service.editBuilding(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(JSON.stringify(error.message))
            }

            const message = result.value as IBuildingDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async patchBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            let dto = req.body as IBuildingEditDTO
            dto.code = req.params.id
            const result = await this.service.editBuilding(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(JSON.stringify(error.message))
            }

            const message = result.value as IBuildingDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getBuildings(_: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getBuildings()
            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(JSON.stringify(error.message))
            }

            const message = result.value as IBuildingDTO[]
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getBuildingsByFloors(req: Request, res: Response, next: NextFunction) {
        try {
            const minFloors = parseInt(req.query.minFloors.toString())
            const maxFloors = parseInt(req.query.maxFloors.toString())

            if (minFloors < 0) {
                return res
                    .status(400)
                    .json({ error: 'minFloors cannot have a negative value' })
            }

            if (maxFloors < 0) {
                return res
                    .status(400)
                    .json({ error: 'maxFloors cannot have a negative value' })
            }

            if (minFloors > maxFloors) {
                return res
                    .status(400)
                    .json({ error: 'minFloors cannot be greater than maxFloors' })
            }

            const dto = req.body as IBuildingMinMaxFloorsDTO
            dto.minMaxFloors = { min: minFloors, max: maxFloors }

            const result = await this.service.getBuildingsByFloors(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(JSON.stringify(error.message))
            }

            const message = result.value as IBuildingDTO[]
            return res.json(message).status(200)
        } catch (e) {
            throw e
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
            case ErrorCode.AlreadyExists:
                ret = 422
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
