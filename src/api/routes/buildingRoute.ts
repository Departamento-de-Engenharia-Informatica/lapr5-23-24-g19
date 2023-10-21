import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import IBuildingController from '../../controllers/IControllers/IBuildingController'
/* FIXME: NAPOLEEEEEEEEEEEEEEES */
// import IFloorController from '../../controllers/IControllers/IFloorController'

import config from '../../../config'

const route = Router()

export default (app: Router) => {
    app.use('/buildings', route)

    const buildingController = Container.get(config.controllers.building.name) as IBuildingController
    /* FIXME: NAPOLEEEEEEEEEEEEEEES */
    // const floorController = Container.get(config.controllers.floor.name) as IFloorController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                code: Joi.string().required(),
                name: Joi.string(),
                description: Joi.string(),
                maxFloorDimensions: Joi.object({
                    length: Joi.number().integer().required(),
                    width: Joi.number().integer().required(),
                }),
            }),
        }),
        (req, res, next) => buildingController.createBuilding(req, res, next)


    )

    // route.post(
    //     '/:id/floors',
    //     celebrate({
    //         body: Joi.object({
    //             floorNumber: Joi.number().integer().required(),
    //             description: Joi.string(),
    //         }),
    //     }),
    //     (req, res, next) => floorController.createFloor(req, res, next),
    // )
}
