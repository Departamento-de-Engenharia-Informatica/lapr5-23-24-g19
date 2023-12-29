import { celebrate, Joi } from 'celebrate'
import { Router } from 'express'
import Container from 'typedi'
import config from '../../../config'
import IPathController from '../../controllers/IControllers/IPathController'
import { withAnyRole } from '../middlewares/authorization'
import { customJwtMiddleware, RolesEnum } from '../middlewares/isAuth'

const route = Router()

export default (app: Router) => {
    app.use('/paths', route)

    const ctrl = Container.get(config.controllers.path.name) as IPathController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                criteria: Joi.string().required(),
                roomStart: Joi.object({
                    building: Joi.string().required(),
                    floor: Joi.number().integer().required(),
                    name: Joi.string().required(),
                }).required(),
                roomGoal: Joi.object({
                    building: Joi.string().required(),
                    floor: Joi.number().integer().required(),
                    name: Joi.string().required(),
                }).required(),
            }),
        }),
        // customJwtMiddleware,
        // withAnyRole([RolesEnum.TASK_MNG]),
        (req, res, next) => ctrl.pathsBetweenBuildings(req, res, next),
    )

    route.get('/criteria', (req, res, next) => ctrl.getPathCriteria(req, res, next))
}
