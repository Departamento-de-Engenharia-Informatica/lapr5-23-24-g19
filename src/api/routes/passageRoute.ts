import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import IBuildingController from '../../controllers/IControllers/IBuildingController'
import IFloorController from '../../controllers/IControllers/IFloorController'

import config from '../../../config'
import IPassageController from '../../controllers/IControllers/IPassageController'

const route = Router()

export default (app: Router) => {
    app.use('/passages', route)

    const PassageController = Container.get(config.controllers.passage.name) as IPassageController

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
        (req, res, next) => PassageController.createPassage(req, res, next)


    )
    
}
