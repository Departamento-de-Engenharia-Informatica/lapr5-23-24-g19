import config from '../../config'
import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'

import IElevatorController from './IControllers/IElevatorController'
import IElevatorService, {
    ErrorResult,
    ErrorCode,
} from '../services/IServices/IElevatorService'
import { IElevatorDTO } from '../dto/IElevatorDTO'
import { ICreatedElevatorDTO } from '../dto/ICreatedElevatorDTO'

@Service()
export default class ElevatorController implements IElevatorController {
    constructor(
        @Inject(config.services.elevator.name) private service: IElevatorService,
    ) {}

    async createElevator(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingId = req.params.id

            const body = req.body as IElevatorDTO
            body.buildingId = buildingId

            const result = await this.service.createElevator(body as IElevatorDTO)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as ICreatedElevatorDTO
            return res.status(201).send(message)
        } catch (err) {
            return next(err)
        }
    }

    public async putElevator(req: Request, res: Response, next: NextFunction) {
        try {
            const firstId = req.params.idb
            const secondId = req.params.ide

            const dto = req.body as IElevatorDTO
            dto.buildingId = firstId

            const result = await this.service.editElevator(
                parseInt(secondId),
                req.body as IElevatorDTO,
            )

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IElevatorDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async patchElevator(req: Request, res: Response, next: NextFunction) {
        try {
            const firstId = req.params.idb
            const secondId = req.params.ide

            const dto = req.body as IElevatorDTO
            dto.buildingId = firstId

            const result = await this.service.editElevator(
                parseInt(secondId),
                req.body as IElevatorDTO,
            )

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IElevatorDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getElevators(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getElevators(req.params.id)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IElevatorDTO[]
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: ErrorCode) {
        let ret: number
        switch (result) {
            case ErrorCode.BussinessRuleViolation:
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
