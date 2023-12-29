import { celebrate, Joi } from 'celebrate'
import { Router } from 'express'
import { Container } from 'typedi'

import IBuildingController from '../../controllers/IControllers/IBuildingController'
import IFloorController from '../../controllers/IControllers/IFloorController'

import config from '../../../config'
import IElevatorController from '../../controllers/IControllers/IElevatorController'
import IFloorMapController from '../../controllers/IControllers/IFloorMapController'
import IRoomController from '../../controllers/IControllers/IRoomController'
import { customJwtMiddleware, RolesEnum } from '../middlewares/isAuth'
import { withAnyRole } from '../middlewares/authorization'

const route = Router()

export default (app: Router) => {
    app.use('/buildings', route)

    // prettier-ignore
    const buildingController = Container.get(config.controllers.building.name) as IBuildingController
    // prettier-ignore
    const floorController = Container.get(config.controllers.floor.name) as IFloorController
    // prettier-ignore
    const floorMapCtrl = Container.get(config.controllers.floorMap.name) as IFloorMapController
    // prettier-ignore
    const elevatorCtrl = Container.get(config.controllers.elevator.name) as IElevatorController
    // prettier-ignore
    const roomCtrl = Container.get(config.controllers.room.name) as IRoomController

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
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => buildingController.createBuilding(req, res, next),
    )

    route.post(
        '/:id/floors',
        celebrate({
            body: Joi.object({
                floorNumber: Joi.number().integer().required(),
                description: Joi.string(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => floorController.createFloor(req, res, next),
    )

    route.get(
        '/:id/floors',
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG, RolesEnum.TASK_MNG, RolesEnum.CLIENT]),
        (req, res, next) => floorController.getFloors(req, res, next),
    )

    route.patch(
        '/:id/floors/:floor',
        celebrate({
            body: Joi.object({
                floorNumber: Joi.number().integer(),
                description: Joi.string(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => floorController.patchFloor(req, res, next),
    )

    route.put(
        '/:id/floors/:floor',
        celebrate({
            body: Joi.object({
                floorNumber: Joi.number().integer().required(),
                description: Joi.string(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => floorController.putFloor(req, res, next),
    )

    route.patch(
        '/:id',
        celebrate({
            body: Joi.object({
                name: Joi.string(),
                description: Joi.string(),
                maxFloorDimensions: Joi.object({
                    length: Joi.number().integer(),
                    width: Joi.number().integer(),
                }),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => buildingController.patchBuilding(req, res, next),
    )

    route.put(
        '/:id',
        celebrate({
            body: Joi.object({
                name: Joi.string(),
                description: Joi.string(),
                maxFloorDimensions: Joi.object({
                    length: Joi.number().integer().required(),
                    width: Joi.number().integer().required(),
                }).required(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => buildingController.putBuilding(req, res, next),
    )

    route.post(
        '/:id/elevators',
        celebrate({
            body: Joi.object({
                floors: Joi.array().items(Joi.number().integer().required()).required(),
                brand: Joi.string(),
                model: Joi.string(),
                serialNumber: Joi.string(),
                description: Joi.string(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => elevatorCtrl.createElevator(req, res, next),
    )

    route.patch(
        '/:idb/elevators/:ide',
        celebrate({
            body: Joi.object({
                floors: Joi.array().items(Joi.number().integer().required()).optional(),
                brand: Joi.string(),
                model: Joi.string(),
                serialNumber: Joi.string(),
                description: Joi.string(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => elevatorCtrl.patchElevator(req, res, next),
    )

    route.put(
        '/:idb/elevators/:ide',
        celebrate({
            body: Joi.object({
                floors: Joi.array().items(Joi.number().integer().required()).required(),
                brand: Joi.string(),
                model: Joi.string(),
                serialNumber: Joi.string(),
                description: Joi.string(),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => elevatorCtrl.putElevator(req, res, next),
    )

    route.post(
        '/:buildingId/floors/:floorNumber/rooms',
        celebrate({
            body: Joi.object({
                name: Joi.string().required(),
                description: Joi.string().required(),
                category: Joi.string().required(),
                dimensions: Joi.object({
                    length: Joi.number().integer(),
                    width: Joi.number().integer(),
                }),
                positions: Joi.object({
                    x: Joi.number().integer(),
                    y: Joi.number().integer(),
                }),
            }),
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => roomCtrl.createRoom(req, res, next),
    )

    route.get(
        '/:buildingId/floors/:floorNumber/rooms',
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG, RolesEnum.TASK_MNG, RolesEnum.CLIENT]),
        (req, res, next) => roomCtrl.getRooms(req, res, next),
    )

    route.get(
        '/:id/elevators',
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => elevatorCtrl.getElevators(req, res, next),
    )

    // const conf = {
    //     decryptionKey: "LaWXgENPlVK0GcKLP2jv1HtL0iwXHZMSfcUNH8iD0LabOJkwgbpq8aI1CoBl-1ok"
    // } as JWEDecryptionConfig

    route.get(
        '',
        celebrate({
            query: {
                minFloors: Joi.number().integer(),
                maxFloors: Joi.number().integer(),
            },
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG, RolesEnum.TASK_MNG, RolesEnum.CLIENT]),
        (req, res, next) => {
            if (
                (req.query.minFloors && req.query.maxFloors) ||
                parseInt(req.query.minFloors as string) == 0 ||
                parseInt(req.query.maxFloors as string) == 0
            ) {
                return buildingController.getBuildingsByFloors(req, res, next)
            } else {
                return buildingController.getBuildings(req, res, next)
            }
        },
    )

    route.patch(
        '/:id/floors/:floorNumber/map',
        celebrate({
            body: Joi.object({
                map: {
                    dimensions: Joi.object({
                        length: Joi.number().required(),
                        width: Joi.number().required(),
                    }),
                    mapContent: Joi.array().items(
                        Joi.array().items(Joi.number().required()),
                    ),
                    passages: Joi.array().items({
                        x: Joi.number().required(),
                        y: Joi.number().required(),
                        orientation: Joi.string().required(),
                        to: Joi.object({
                            building: Joi.string().required(),
                            floor: Joi.number().required(),
                        }),
                    }),
                    elevators: Joi.array().items({
                        x: Joi.number().required(),
                        y: Joi.number().required(),
                        orientation: Joi.string().required(),
                        floors: Joi.array().items(Joi.number().required()),
                    }),
                    rooms: Joi.array().items({
                        x: Joi.number().required(),
                        y: Joi.number().required(),
                        orientation: Joi.string().required(),
                        name: Joi.string().required(),
                    }),
                },
            }).unknown(true), // This allows additional properties in the body
        }),
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => floorMapCtrl.updateMap(req, res, next),
    )

    route.get('/:id/floors/:floorNumber/map', (req, res, next) =>
        floorMapCtrl.getMap(req, res, next),
    )

    route.get(
        '/:id/floors/passages',
        customJwtMiddleware,
        withAnyRole([RolesEnum.CAMPUS_MNG]),
        (req, res, next) => floorController.floorsWithPassage(req, res, next),
    )
}
