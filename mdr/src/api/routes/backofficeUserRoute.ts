import config from '../../../config'
import { Container } from 'typedi'
import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'

import IBackofficeUserController from '../../controllers/IControllers/IBackofficeUserController'
import { customJwtMiddleware, RolesEnum } from '../middlewares/isAuth'
import { withAnyRole } from '../middlewares/authorization'

const route = Router()

export default (app: Router) => {
    app.use('/users-backoffice', route)

    const ctrl = Container.get(
        config.controllers.backofficeUser.name,
    ) as IBackofficeUserController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                name: Joi.string().required(),
                role: Joi.string().required(),
                email: Joi.string().required(),
                phoneNumber: Joi.string()
                    .regex(/^[0-9]+$/)
                    .required(),
                password: Joi.string().required(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.ADMIN]),
        (req, res, next) => ctrl.createBackofficeUser(req, res, next),
    )
}
