import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import config from '../../../config'
import IPassageController from '../../controllers/IControllers/IPassageController'

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
                    floorNumber: Joi.number()
                        .integer()
                        .required(),
                }),
                floor2: Joi.object({
                    buildingCode: Joi.string().required(),
                    floorNumber: Joi.number()
                        .integer()
                        .required(),
                }),
            }),
        }),
        (req, res, next) => ctrl.createPassage(req, res, next),
    )

    route.get(
        '/:id/:id',
        celebrate({
            body: Joi.object({
                building1: Joi.object({
                    name: Joi.string(),
                    description: Joi.string(),
                    maxFloorDimensions: Joi.object({
                        length: Joi.number()
                            .integer()
                            .required(),
                        width: Joi.number()
                            .integer()
                            .required(),
                    }),
                }).optional(),
                building2: Joi.object({
                    name: Joi.string(),
                    description: Joi.string(),
                    maxFloorDimensions: Joi.object({
                        length: Joi.number()
                            .integer()
                            .required(),
                        width: Joi.number()
                            .integer()
                            .required(),
                    }),
                }).optional(),
            }).or('building1', 'building2'),
        }),
        (req, res, next) => ctrl.getPassages(req, res, next),
    )

    route.put(
        '',
        celebrate({
            body: Joi.object({
                old: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }).required(),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }).required(),
                }),
                new: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }).required(),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }).required(),
                }),
            }),
        }),
        (req, res, next) => ctrl.editPassage(req, res, next),
    )

    route.patch(
        '',
        celebrate({
            body: Joi.object({
                old: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }).required(),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }).required(),
                }),
                new: Joi.object({
                    floor1: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }),
                    floor2: Joi.object({
                        buildingCode: Joi.string().required(),
                        floorNumber: Joi.number()
                            .integer()
                            .required(),
                    }),
                }),
            }),
        }),
        (req, res, next) => ctrl.editPassage(req, res, next),
    )
}
