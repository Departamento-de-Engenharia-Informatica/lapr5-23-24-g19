import { Router } from 'express'
import { Container } from 'typedi'
import IRoleController from '../../controllers/IControllers/IRoleController'

import config from '../../../config'
import { customJwtMiddleware, RolesEnum } from '../middlewares/isAuth'
import { withAnyRole } from '../middlewares/authorization'

const route = Router()

export default (app: Router) => {
    app.use('/roles', route)

    const ctrl = Container.get(config.controllers.role.name) as IRoleController

    route.get(
        '',
        customJwtMiddleware,
        withAnyRole([RolesEnum.ADMIN]),
        (req, res, next) => ctrl.getRoles(req, res, next)
    )

    // route.post(
    //     '',
    //     celebrate({
    //         body: Joi.object({
    //             name: Joi.string().required(),
    //         }),
    //     }),
    //     (req, res, next) => ctrl.createRole(req, res, next),
    // )

    // route.put(
    //     '',
    //     celebrate({
    //         body: Joi.object({
    //             id: Joi.string().required(),
    //             name: Joi.string().required(),
    //         }),
    //     }),
    //     (req, res, next) => ctrl.updateRole(req, res, next),
    // )
}
