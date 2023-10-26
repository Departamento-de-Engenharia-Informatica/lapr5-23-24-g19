import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import { Result } from '../core/logic/Result'
import IPassageController from './IControllers/IPassageController'
import IPassageService from '../services/IServices/IPassageService'
import { IPassageDTO } from '../dto/IPassageDTO'
import IUpdatePassageDTO from '../dto/IUpdatePassageDTO'
import { parseInt } from 'lodash'

@Service()
export default class PassageController implements IPassageController {
    constructor(@Inject(config.services.passage.name) private passageServiceInstance: IPassageService) {}

    public async createPassage(req: Request, res: Response, next: NextFunction) {
        try {
            const roleOrError = (await this.passageServiceInstance.createPassage(req.body as IPassageDTO)) as Result<
                IPassageDTO
            >

            if (roleOrError.isFailure) {
                return res.status(402).send()
            }

            const floorDTO = roleOrError.getValue()
            return res.json(floorDTO).status(201)
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
                if (result.isFailure) {
                    return res.status(422).send()
                }
                return res.json(result.getValue()).status(200)
            } else if (building1 && building2) {
                const result = await this.passageServiceInstance.getPassagesBetweenBuildings(building1, building2)
                if (result.isFailure) {
                    return res.status(422).send()
                }
                return res.json(result.getValue()).status(200)
            } else {
                // Handle the case of having exactly 1 building here
                return res.status(400).json({ error: 'You must provide either 0 or 2 buildings' })
            }
        } catch (e) {
            return next(e)
        }
    }

    async editPassage(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.passageServiceInstance.editPassage(req.body as IUpdatePassageDTO)

            if (result.isFailure) {
                return res
                    .json(result.errorValue())
                    .status(404)
                    .send()
            }

            return res.json(result.getValue()).status(200)
        } catch (e) {
            next(e)
        }
    }
}
