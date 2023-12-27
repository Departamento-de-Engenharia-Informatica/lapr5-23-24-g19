import {Router} from 'express'
import {Container} from 'typedi'
import config from '../../../config'

import ITaskController from '../../controllers/IControllers/ITaskController'
import {celebrate, Joi} from 'celebrate'
import {customJwtMiddleware, isBackoffice, isClient, RolesEnum} from '../middlewares/isAuth'

const route = Router()

export default (app: Router) => {
    app.use('/task', route)

    const ctrl = Container.get(config.controllers.task.name) as ITaskController

    route.get('/types', customJwtMiddleware, (req, res, next) =>
        ctrl.getTypes(req, res, next),
    )

    route.post(
        '/surveillance',
        customJwtMiddleware,
        isClient(),
        (req, res, next) => ctrl.createSurveillanceTask(req, res, next),
    )

    route.post(
        '/delivery',
        customJwtMiddleware,
        isClient(),
        (req, res, next) => ctrl.createDeliveryTask(req, res, next),
    )

    route.get(
        '',
        celebrate({
            query: {
                status: Joi.string(),
            },
        }),
        // customJwtMiddleware,
        //isBackoffice([RolesEnum.TASK_MNG]),
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
        isBackoffice([RolesEnum.TASK_MNG]),
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
        //customJwtMiddleware,
        //isBackoffice([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.taskSequence(req, res, next),
    )

    route.get(
        '/sequence/algorithms',
        //customJwtMiddleware,
        //isBackoffice([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.taskSequenceAlgorithms(req, res, next),
    )

    route.patch(
        '/:id',
        celebrate({
            body: Joi.object({
                taskStatus: Joi.string(),
            }).unknown(true), // This allows additional properties in the body
        }),
        //customJwtMiddleware,
        //isBackoffice([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.updateTask(req, res, next),
    )
}
