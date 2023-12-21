import { Router } from 'express'
import { Container } from 'typedi'
import config from '../../../config'

import ITaskController from '../../controllers/IControllers/ITaskController'
import { filter } from 'lodash'
import { Joi, celebrate } from 'celebrate'

const route = Router()

export default (app: Router) => {
    app.use('/task', route)

    const ctrl = Container.get(config.controllers.task.name) as ITaskController

    route.get('/types', (req, res, next) => ctrl.getTypes(req, res, next))

    route.post('/surveillance', (req, res, next) =>
        ctrl.createSurveillanceTask(req, res, next),
    )

    route.post('/delivery', (req, res, next) => ctrl.createDeliveryTask(req, res, next))
    
    route.get(
        '/filter',
        celebrate({
            query: {
                criteria: Joi.string(),
                rule: Joi.string(),
            },
        }),

        (req, res, next) => {
                return ctrl.getByFilter(req, res, next)
        }
    )
}
