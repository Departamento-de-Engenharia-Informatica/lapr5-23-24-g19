import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IRoomController from './IControllers/IRoomController'
import { IRoomDTO } from '../dto/IRoomDTO'
import IRoomService from '../services/IServices/IRoomService'

@Service()
export default class RoomController implements IRoomController {
    constructor(@Inject(config.services.room.name) private service: IRoomService) {}

    public async createRoom(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.createRoom(req.body as IRoomDTO)

            if (result.isFailure) {
                return res.status(422).send()
            }

            return res.json(result.getValue()).status(201)
        } catch (e) {
            return next(e)
        }
    }
}
