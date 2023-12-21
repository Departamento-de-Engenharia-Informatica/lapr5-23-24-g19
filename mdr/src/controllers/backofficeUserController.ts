import config from '../../config'
import { Inject, Service } from 'typedi'
import { Request, Response, NextFunction } from 'express'

import IBackofficeUserController from './IControllers/IBackofficeUserController'
import { IBackofficeUserDTO } from '../dto/IBackofficeUserDTO'
import { ICreatedBackofficeUserDTO } from '../dto/ICreatedBackofficeUserDTO'
import IBackofficeUserService, {
    BackofficeUserErrorCode,
    BackofficeUserErrorResult,
} from '../services/IServices/IBackofficeUserService'

@Service()
export default class BackofficeUserController implements IBackofficeUserController {
    constructor(
        @Inject(config.services.backofficeUser.name)
        private service: IBackofficeUserService,
    ) {}

    async createBackofficeUser(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IBackofficeUserDTO
            const result = await this.service.createBackofficeUser(dto)

            if (result.isLeft()) {
                const err = result.value as BackofficeUserErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as ICreatedBackofficeUserDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: BackofficeUserErrorCode) {
        let ret: number
        switch (result) {
            case BackofficeUserErrorCode.BussinessRuleViolation:
                ret = 422
                break
            case BackofficeUserErrorCode.NotFound:
                ret = 404
                break
            case BackofficeUserErrorCode.AlreadyExists:
                ret = 422
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
