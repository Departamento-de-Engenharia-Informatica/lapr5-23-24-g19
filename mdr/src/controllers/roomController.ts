import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IRoomController from './IControllers/IRoomController'
import { IRoomDTO } from '../dto/IRoomDTO'
import IRoomService, { ErrorCode, ErrorResult } from '../services/IServices/IRoomService'

@Service()
export default class RoomController implements IRoomController {
    constructor(@Inject(config.services.room.name) private service: IRoomService) {}

    public async createRoom(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IRoomDTO
            dto.buildingCode = req.params.buildingId
            dto.floorNumber = parseInt(req.params.floorNumber)

            const result = await this.service.createRoom(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IRoomDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    public async getRooms(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IRoomDTO
            dto.buildingCode = req.params.buildingId
            dto.floorNumber = parseInt(req.params.floorNumber)

            const result = await this.service.getRooms(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IRoomDTO[]
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
            case ErrorCode.AlreadyExists:
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
