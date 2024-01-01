import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'
import IRoleService from '../services/IServices/IRoleService'
import IRoleController from './IControllers/IRoleController'

@Service()
export default class RoleController implements IRoleController {
    constructor(
        @Inject(config.services.role.name) private service: IRoleService,
    ) { }

    createRole(_req: Request, _res: Response, _next: NextFunction) {
        throw new Error('Method not implemented.')
    }
    updateRole(_req: Request, _res: Response, _next: NextFunction,) {
        throw new Error('Method not implemented.')
    }

    async getRoles(_req: Request, res: Response, next: NextFunction) {
        try {
            const roles = await this.service.getRoles()

            if (roles.isFailure) {
                return res.status(500).send(roles.errorValue())
            }

            return res.status(200).send(roles.getValue())
        } catch (e) {
            return next(e)
        }
    }
}
