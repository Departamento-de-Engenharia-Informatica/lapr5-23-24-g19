import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import IFloorController from '../../controllers/IControllers/IFloorController'
import config from '../../../config'

const route = Router()

export default (app: Router) => {
    app.use('/building', route)

    const ctrl = Container.get(config.controllers.floor.name) as IFloorController

    route.post(
        '/:id/floors',
        celebrate({
            body: Joi.object({
                floorNumber: Joi.number().integer().required(),
                description: Joi.string(),
            }),
        }),
        (req, res, next) => ctrl.createFloor(req, res, next),
    )
}
