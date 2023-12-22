import { celebrate, Joi } from 'celebrate'
import { Router } from 'express'
import { Container } from 'typedi'
import config from '../../../config'

import IClientController from '../../controllers/IControllers/IClientController'

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

    route.get('/:email', (req, res, next) => ctrl.getClient(req, res, next))

    route.delete(
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
                authToken: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.deleteClient(req, res, next),
    )
}
