import { Router } from 'express'
import { Container } from 'typedi'
import config from '../../../config'

import ITaskController from '../../controllers/IControllers/ITaskController'
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
        '',
        celebrate({
            query: {
                status: Joi.string(),
            },
        }),

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

        (req, res, next) => {
            return ctrl.getByFilter(req, res, next)
        },
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

    route.patch(
        '/sequence',
        celebrate({
            body: Joi.object({
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
}
