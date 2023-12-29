import { celebrate, Joi } from 'celebrate'
import { Router } from 'express'
import { Container } from 'typedi'
import config from '../../../config'

import IClientController from '../../controllers/IControllers/IClientController'
import {customJwtMiddleware, isBackoffice, isClient, requireReAuth, RolesEnum} from '../middlewares/isAuth'

const route = Router()

export default (app: Router) => {
    app.use('/clients', route)

    const ctrl = Container.get(config.controllers.client.name) as IClientController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                name: Joi.string().required(),
                email: Joi.string().required(),
                phoneNumber: Joi.string()
                    .regex(/^[0-9]+$/)
                    .required(),
                vatNumber: Joi.number()
                    .integer()
                    .required(),
                password: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.createClient(req, res, next),
    )

    route.post(
        '/data',
        celebrate({
            body: Joi.object({
                email: Joi.string().required(),
                password: Joi.string().required(),
            })
        }),
        // customJwtMiddleware,
        // isClient(),
        // requireReAuth(),
        (req, res, next) => ctrl.exportClientData(req, res, next),
    )

    route.get('/:email',
        customJwtMiddleware,
        (req, res, next) => ctrl.getClient(req, res, next))

    route.get(
        '',
        celebrate({ query: Joi.object({ state: Joi.string() }) }),
        // customJwtMiddleware,
        // isBackoffice([RolesEnum.ADMIN]),
        (req, res, next) => ctrl.getClientsByState(req, res, next),
    )

    route.patch(
        '',
        celebrate({
            body: Joi.object({
                state: Joi.string().required(),
                email: Joi.string().required(),
            }),
        }),
        // customJwtMiddleware,
        // isBackoffice([RolesEnum.ADMIN]),
        (req, res, next) => ctrl.updateClientState(req, res, next),
    )

    route.patch(
        '/:email',
        celebrate({
            body: Joi.object({
                name: Joi.string(),
                phoneNumber: Joi.string().regex(/^[0-9]+$/),
                vatNumber: Joi.number().integer(),
            }),
        }),
        customJwtMiddleware,
        isClient(),
        (req, res, next) => ctrl.patchClient(req, res, next),
    )

    route.delete('/:email', customJwtMiddleware, isClient(), (req, res, next) =>
        ctrl.deleteClient(req, res, next),
    )
}
