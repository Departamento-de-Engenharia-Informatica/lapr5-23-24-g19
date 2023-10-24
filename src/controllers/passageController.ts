import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import { Result } from '../core/logic/Result'
import IPassageController from './IControllers/IPassageController'
import IPassageService from '../services/IServices/IPassageService'
import { IPassageDTO } from '../dto/IPassageDTO'
import { IBuildingDTO } from '../dto/IBuildingDTO'

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
            const building1 = req.body.building1
            const building2 = req.body.building2

            // Either 0 or 2 buildings are present
            if (!building1 && !building2) {
                const result = await this.passageServiceInstance.getAllPassages()
                if (result.isFailure) {
                    return res.status(422).send()
                }
                return res.json(result.getValue()).status(200)
            } else if (building1 && building2) {
                const firstId = req.params.id
                const secondId = req.params.id

                const result = await this.passageServiceInstance.getPassagesBetweenBuildings(firstId, secondId)
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
}
