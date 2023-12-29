import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import config from '../../../config'
import IRobotTypeController from '../../controllers/IControllers/IRobotTypeController'
import { customJwtMiddleware, RolesEnum } from '../middlewares/isAuth'
import { withAnyRole } from '../middlewares/authorization'

const route = Router()

export default (app: Router) => {
    app.use('/robottypes', route)

    const robotTypeController = Container.get(
        config.controllers.robotType.name,
    ) as IRobotTypeController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                code: Joi.string().required(),
                brand: Joi.string().required(),
                model: Joi.string().required(),
                taskTypes: Joi.array().items(Joi.string()).min(1).required(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.FLEET_MNG]),
        (req, res, next) => robotTypeController.createRobotType(req, res, next),
    )

    route.get(
        '',
        customJwtMiddleware,
        withAnyRole([RolesEnum.FLEET_MNG]),
        (req, res, next) => robotTypeController.getRobotTypes(req, res, next),
    )
}
