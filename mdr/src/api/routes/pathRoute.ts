import { celebrate, Joi } from 'celebrate'
import { Router } from 'express'
import Container from 'typedi'
import config from '../../../config'
import IPathController from '../../controllers/IControllers/IPathController'

const route = Router()

export default (app: Router) => {
    app.use('/paths', route)

    const ctrl = Container.get(config.controllers.path.name) as IPathController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                criteria: Joi.string().required(),
                start: Joi.object({
                    building: Joi.string().required(),
                    floor: Joi.number().integer().required(),
                    coordinates: Joi.object({
                        x: Joi.number().integer().required(),
                        y: Joi.number().integer().required(),
                    }).required(),
                }).required(),
                goal: Joi.object({
                    building: Joi.string().required(),
                    floor: Joi.number().integer().required(),
                    coordinates: Joi.object({
                        x: Joi.number().integer().required(),
                        y: Joi.number().integer().required(),
                    }).required(),
                }).required(),
            }),
        }),
        (req, res, next) => ctrl.pathsBetweenBuildings(req, res, next),
    )

    route.get('/criteria', (req, res, next) => ctrl.getPathCriteria(req, res, next))
}
