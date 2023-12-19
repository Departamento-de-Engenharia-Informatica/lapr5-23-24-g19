import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IPassageController from './IControllers/IPassageController'
import IPassageService, {
    ErrorResult,
    ErrorCode,
} from '../services/IServices/IPassageService'
import { IPassageDTO } from '../dto/IPassageDTO'
import IUpdatePassageDTO from '../dto/IUpdatePassageDTO'

@Service()
export default class PassageController implements IPassageController {
    constructor(
        @Inject(config.services.passage.name)
        private passageServiceInstance: IPassageService,
    ) {}

    public async createPassage(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.passageServiceInstance.createPassage(
                req.body as IPassageDTO,
            )

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IPassageDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    public async getPassages(req: Request, res: Response, next: NextFunction) {
        try {
            const building1 = req.query.building1 as string | undefined
            const building2 = req.query.building2 as string | undefined

            // Less than two buildings specified
            if (!building1 || !building2) {
                const result = await this.passageServiceInstance.getAllPassages()
                if (result.isLeft()) {
                    const error = result.value as ErrorResult
                    const ret: number = this.resolveHttpCode(error.errorCode)
                    return res.status(ret).send(error.message)
                }
                const message = result.value as IPassageDTO[]
                return res.json(message).status(200)
            } else if (building1 && building2) {
                const result =
                    await this.passageServiceInstance.getPassagesBetweenBuildings(
                        building1,
                        building2,
                    )
                if (result.isLeft()) {
                    const error = result.value as ErrorResult
                    const ret: number = this.resolveHttpCode(error.errorCode)
                    return res.status(ret).send(error.message)
                }
                const message = result.value as IPassageDTO[]
                return res.json(message).status(200)
            } else {
                // Handle the case of having exactly 1 building here
                return res
                    .status(400)
                    .json({ error: 'You must provide either 0 or 2 buildings' })
            }
        } catch (e) {
            return next(e)
        }
    }

    async editPassage(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.passageServiceInstance.editPassage(
                req.body as IUpdatePassageDTO,
            )

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                const ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IPassageDTO
            return res.json(message).status(200)
        } catch (e) {
            next(e)
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
