import { Router } from 'express'
import { Container } from 'typedi'
import config from '../../../config'

import ITaskController from '../../controllers/IControllers/ITaskController'
import { Joi, celebrate } from 'celebrate'
import { customJwtMiddleware } from '../middlewares/isAuth'

const route = Router()

export default (app: Router) => {
    app.use('/task', route)

    const ctrl = Container.get(config.controllers.task.name) as ITaskController

    route.get('/types', customJwtMiddleware, (req, res, next) =>
        ctrl.getTypes(req, res, next),
    )

    route.post('/surveillance', customJwtMiddleware, (req, res, next) =>
        ctrl.createSurveillanceTask(req, res, next),
    )

    route.post('/delivery', customJwtMiddleware, (req, res, next) =>
        ctrl.createDeliveryTask(req, res, next),
    )

    route.get(
        '',
        celebrate({
            query: {
                status: Joi.string(),
            },
        }),
        customJwtMiddleware,
        (req, res, next) => {
            return ctrl.getByStatus(req, res, next)
        },
    )

    route.get(
        '/filter',
        celebrate({
            query: {
                criteria: Joi.string(),
                rule: Joi.string(),
            },
        }),
        customJwtMiddleware,
        (req, res, next) => {
            return ctrl.getByFilter(req, res, next)
        },
    )

    route.patch(
        '/sequence',
        celebrate({
            body: Joi.object({
                algorithm: Joi.string().required(),
                tasks: Joi.array()
                    .items(
                        Joi.object({
                            id: Joi.string().required(),
                            type: Joi.string().required(),
                        }),
                    )
                    .required(),
            }),
        }),
        (req, res, next) => ctrl.taskSequence(req, res, next),
    )

    route.patch(
        '/:id',
        celebrate({
            body: Joi.object({
                taskStatus: Joi.string(),
            }).unknown(true), // This allows additional properties in the body
        }),
        (req, res, next) => ctrl.updateTask(req, res, next),
    )
}
