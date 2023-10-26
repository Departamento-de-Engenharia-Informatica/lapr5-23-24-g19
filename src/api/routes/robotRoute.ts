import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import config from '../../../config'
import IRobotController from '../../controllers/IControllers/IRobotController'

const route = Router()

export default (app: Router) => {
    app.use('/robots', route)

    const ctrl = Container.get(config.controllers.robotType.name) as IRobotController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                code: Joi.string().required(),
                nickname: Joi.string().required(),
                typeCode: Joi.string().required(),
                serialNumber: Joi.string().required(),
                description: Joi.string(),
            }),
        }),
        (req, res, next) => ctrl.createRobot(req, res, next),
    )
}
