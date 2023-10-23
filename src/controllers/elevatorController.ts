import config from '../../config'

import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import IElevatorController from './IControllers/IElevatorController'
import IElevatorService from '../services/IServices/IElevatorService'
import { IElevatorDTO } from '../dto/IElevatorDTO'


@Service()
export default class ElevatorController implements IElevatorController {

    constructor(
        @Inject(config.services.elevator.name) private service: IElevatorService
    ) {}


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
}
