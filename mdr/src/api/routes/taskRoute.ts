import { Router } from 'express'
import { Container } from 'typedi'
import config from '../../../config'

import ITaskController from '../../controllers/IControllers/ITaskController'

const route = Router()

export default (app: Router) => {
    app.use('/task', route)

    const ctrl = Container.get(config.controllers.task.name) as ITaskController

    route.get('/types', (req, res, next) => ctrl.getTypes(req, res, next))

    route.post('/surveillance', (req, res, next) =>
        ctrl.createSurveillanceTask(req, res, next),
    )

    route.post('/delivery', (req, res, next) => ctrl.createDeliveryTask(req, res, next))
}
