import config from '../../../config'
import { Container } from 'typedi'
import { Router } from 'express'

import ITaskController from '../../controllers/IControllers/ITaskController'

const route = Router()

export default (app: Router) => {
    app.use('/task', route)

    const ctrl = Container.get(config.controllers.task.name) as ITaskController

    route.get('/types', (req, res, next) => ctrl.getTypes(req, res, next))
}
