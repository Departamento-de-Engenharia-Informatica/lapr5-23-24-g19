import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import config from '../../../config'
import IPassageController from '../../controllers/IControllers/IPassageController'
import { customJwtMiddleware, RolesEnum } from '../middlewares/isAuth'
import { withAnyRole } from '../middlewares/authorization'

const route = Router()

export default (app: Router) => {
    app.use('/passages', route)

    const ctrl = Container.get(config.controllers.passage.name) as IPassageController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                floor1: Joi.object({
                    buildingCode: Joi.string().required(),
                    floorNumber: Joi.number().integer().required(),
                }),
                floor2: Joi.object({
                    buildingCode: Joi.string().required(),
                    floorNumber: Joi.number().integer().required(),
                }),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => ctrl.createPassage(req, res, next),
    )

    route.get(
        '',
        celebrate({
            query: {
                building1: Joi.string(),
                building2: Joi.string(),
            },
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => ctrl.getPassages(req, res, next),
    )

    route.put(
        '',
        celebrate({
            body: Joi.object({
                old: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }).required(),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }).required(),
                }),
                new: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }).required(),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }).required(),
                }),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => ctrl.editPassage(req, res, next),
    )

    route.patch(
        '',
        celebrate({
            body: Joi.object({
                old: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }).required(),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }).required(),
                }),
                new: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number().integer().required(),
                    }),
                }),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => ctrl.editPassage(req, res, next),
    )
}
