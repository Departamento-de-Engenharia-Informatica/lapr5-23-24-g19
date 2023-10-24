import config from '../../config'

import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import IElevatorController from './IControllers/IElevatorController'
import IElevatorService from '../services/IServices/IElevatorService'
import { IElevatorDTO } from '../dto/IElevatorDTO'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { ElevatorIdentifier } from '../domain/elevator/identifier'

@Service()
export default class ElevatorController implements IElevatorController {
    constructor(@Inject(config.services.elevator.name) private service: IElevatorService) {}

    async createElevator(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingId = req.params.id

            const body = req.body as IElevatorDTO
            body.buildingId = buildingId

            const result = await this.service.createElevator(body as IElevatorDTO)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(201)
        } catch (err) {
            return next(err)
        }
    }

    public async putElevator(req: Request, res: Response, next: NextFunction) {
        try {
            const firstId = req.params.id
            const secondId = req.params.id

            const dto = req.body as IElevatorDTO
            dto.buildingId = firstId
            dto.identifier = parseInt(secondId)

            const result = await this.service.createElevator(dto)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async patchElevator(req: Request, res: Response, next: NextFunction) {
        try {
            const firstId = req.params.id
            const secondId = req.params.id

            const dto = req.body as IElevatorDTO
            dto.buildingId = firstId

            const result = await this.service.editElevator(secondId, req.body as IElevatorDTO)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getElevators(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getElevators(req.params.id)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }
}
