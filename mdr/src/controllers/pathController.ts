import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'
import { IPathBetweenBuildingsDTO } from '../dto/IPathBetweenBuildingsDTO'
import IPathService from '../services/IServices/IPathService'
import IPathController from './IControllers/IPathController'

@Service()
export default class PathController implements IPathController {
    constructor(@Inject(config.services.path.name) private service: IPathService) {}

    async pathsBetweenBuildings(req: Request, res: Response, next: NextFunction) {
        try {
            console.log('oi')
            const dto = req.body as IPathBetweenBuildingsDTO
            const result = await this.service.pathsBetweenBuildings(dto)


            return res.json(result).status(200)
        } catch (e) {
            return next(e)
        }
    }

    async getPathCriteria(_req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getPathCriteria()

            return res.json(result).status(200)
        } catch (e) {
            return next(e)
        }
    }
}
