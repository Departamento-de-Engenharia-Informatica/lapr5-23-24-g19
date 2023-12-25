// import { Request, Response, NextFunction } from 'express'
// import { Inject, Service } from 'typedi'
// import config from '../../config'
//
// import IRoleController from './IControllers/IRoleController'
// import IRoleService from '../services/IServices/IRoleService'
// import IRoleDTO from '../dto/IRoleDTO'
//
// import { Result } from '../core/logic/Result'

import { Request, Response, NextFunction } from 'express'
import { ParamsDictionary } from 'express-serve-static-core'
import { ParsedQs } from 'qs'
import { Inject, Service } from 'typedi'
import config from '../../config'
import IRoleService from '../services/IServices/IRoleService'
import IRoleController from './IControllers/IRoleController'

//
@Service() /* TODO: extends ../core/infra/BaseController */
export default class RoleController implements IRoleController {
    createRole(
        req: Request<ParamsDictionary, any, any, ParsedQs, Record<string, any>>,
        res: Response<any, Record<string, any>>,
        next: NextFunction,
    ) {
        throw new Error('Method not implemented.')
    }
    updateRole(
        req: Request<ParamsDictionary, any, any, ParsedQs, Record<string, any>>,
        res: Response<any, Record<string, any>>,
        next: NextFunction,
    ) {
        throw new Error('Method not implemented.')
    }
    constructor(
        @Inject(config.services.role.name) private roleServiceInstance: IRoleService,
    ) {}
    //
    //     public async createRole(req: Request, res: Response, next: NextFunction) {
    //         try {
    //             const roleOrError = (await this.roleServiceInstance.createRole(
    //                 req.body as IRoleDTO,
    //             )) as Result<IRoleDTO>
    //
    //             if (roleOrError.isFailure) {
    //                 return res.status(402).send()
    //             }
    //
    //             const roleDTO = roleOrError.getValue()
    //             return res.json(roleDTO).status(201)
    //         } catch (e) {
    //             return next(e)
    //         }
    //     }
    //
    //     public async updateRole(req: Request, res: Response, next: NextFunction) {
    //         try {
    //             const roleOrError = (await this.roleServiceInstance.updateRole(
    //                 req.body as IRoleDTO,
    //             )) as Result<IRoleDTO>
    //
    //             if (roleOrError.isFailure) {
    //                 return res.status(404).send()
    //             }
    //
    //             const roleDTO = roleOrError.getValue()
    //             return res.status(201).json(roleDTO)
    //         } catch (e) {
    //             return next(e)
    //         }
    //     }
}
