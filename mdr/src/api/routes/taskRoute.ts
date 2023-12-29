import { Router } from 'express'
import { Container } from 'typedi'
import config from '../../../config'

import ITaskController from '../../controllers/IControllers/ITaskController'
import { celebrate, Joi } from 'celebrate'
import { customJwtMiddleware, RolesEnum } from '../middlewares/isAuth'
import { withAnyRole } from '../middlewares/authorization'

const route = Router()

export default (app: Router) => {
    app.use('/task', route)

    const ctrl = Container.get(config.controllers.task.name) as ITaskController

    route.get(
        '/types',
        customJwtMiddleware,
        withAnyRole([RolesEnum.FLEET_MNG, RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.getTypes(req, res, next),
    )

    route.post(
        '/surveillance',
        customJwtMiddleware,
        withAnyRole([RolesEnum.CLIENT]),
        (req, res, next) => ctrl.createSurveillanceTask(req, res, next),
    )

    route.post(
        '/delivery',
        customJwtMiddleware,
        withAnyRole([RolesEnum.CLIENT]),
        (req, res, next) => ctrl.createDeliveryTask(req, res, next),
    )

    route.get(
        '',
        celebrate({
            query: {
                status: Joi.string(),
            },
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.getByStatus(req, res, next),
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
        withAnyRole([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.getByFilter(req, res, next),
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
        customJwtMiddleware,
        withAnyRole([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.taskSequence(req, res, next),
    )

    route.get(
        '/sequence/algorithms',
        customJwtMiddleware,
        withAnyRole([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.taskSequenceAlgorithms(req, res, next),
    )

    route.patch(
        '/:id',
        celebrate({
            body: Joi.object({
                taskStatus: Joi.string(),
            }).unknown(true), // This allows additional properties in the body
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.updateTask(req, res, next),
    )
}
