import config from '../../../config'
import { Container } from 'typedi'
import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'

import IRobotController from '../../controllers/IControllers/IRobotController'
import middlewares from '../middlewares'
import {customJwtMiddleware, isBackoffice, RolesEnum} from '../middlewares/isAuth'

const route = Router()

export default (app: Router) => {
    app.use('/robots', route)

    const ctrl = Container.get(config.controllers.robot.name) as IRobotController

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
        customJwtMiddleware,
        isBackoffice([RolesEnum.FLEET_MNG]),
        (req, res, next) => ctrl.createRobot(req, res, next),
    )

    route.patch(
        '/:id/inhibit',
        celebrate({
            body: Joi.object({
                state: Joi.number().integer().required(),
            }),
        }),
        customJwtMiddleware,
        isBackoffice([RolesEnum.FLEET_MNG]),
        (req, res, next) => ctrl.inhibitRobot(req, res, next),
    )

    route.get(
        '',
        customJwtMiddleware,
        isBackoffice([RolesEnum.FLEET_MNG]),
        (req, res, next) => ctrl.getRobots(req, res, next),
    )
}
