import config from '../../config'
import { Inject, Service } from 'typedi'
import { Request, Response, NextFunction } from 'express'

import IClientService, {
    ClientErrorCode,
    ClientErrorResult,
} from '../services/IServices/IClientService'
import IClientController from './IControllers/IClientController'
import { IClientDTO } from '../dto/IClientDTO'
import { ICreatedClientDTO } from '../dto/ICreatedClientDTO'

@Service()
export default class ClientController implements IClientController {
    constructor(@Inject(config.services.client.name) private service: IClientService) {}

    async createClient(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IClientDTO
            const result = await this.service.createClient(dto)

            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as ICreatedClientDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async getClient(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getClient(req.params.email as string)
            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as IClientDTO
            return res.status(200).send(message)
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: ClientErrorCode) {
        let ret: number
        switch (result) {
            case ClientErrorCode.BussinessRuleViolation:
                ret = 422
                break
            case ClientErrorCode.NotFound:
                ret = 404
                break
            case ClientErrorCode.AlreadyExists:
                ret = 422
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
