import { Router } from 'express'
import { celebrate, Joi } from 'celebrate'
import { Container } from 'typedi'

import IBuildingController from '../../controllers/IControllers/IBuildingController'
import IFloorController from '../../controllers/IControllers/IFloorController'

import config from '../../../config'
import IElevatorController from '../../controllers/IControllers/IElevatorController'
import IRoomController from '../../controllers/IControllers/IRoomController'
import multer from 'multer'
import path from 'path'

const route = Router()

export default (app: Router) => {
    app.use('/buildings', route)

    //TODO: verify if it is to use fileSystem
    const storage = multer.diskStorage({
        destination: (req, file, callback) => {
            // Specify the directory where you want to store the uploaded files
            callback(null, 'maps/');
        },
        filename: (req, file, callback) => {
            // Use the original filename, but add a timestamp to make it unique
            const timestamp = Date.now();
            const extension = path.extname(file.originalname);
            const uniqueFilename = `${timestamp}${extension}`;
            callback(null, uniqueFilename);
        },
    });
    const upload = multer({ storage: storage });

    const buildingController = Container.get(config.controllers.building.name) as IBuildingController
    const floorController = Container.get(config.controllers.floor.name) as IFloorController
    const elevatorCtrl = Container.get(config.controllers.elevator.name) as IElevatorController
    const roomCtrl = Container.get(config.controllers.room.name) as IRoomController

    route.post(
        '',
        celebrate({
            body: Joi.object({
                code: Joi.string().required(),
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
            }),
        }),
        (req, res, next) => buildingController.createBuilding(req, res, next),
    )

    route.post(
        '/:id/floors',
        celebrate({
            body: Joi.object({
                floorNumber: Joi.number()
                    .integer()
                    .required(),
                description: Joi.string(),
            }),
        }),
        (req, res, next) => floorController.createFloor(req, res, next),
    )

    route.get(
        '/:id/floors',
        (req, res, next) => floorController.getFloors(req, res, next),
    )

    route.patch(
        '/:id/floors/:floor',
        celebrate({
            body: Joi.object({
                floorNumber: Joi.number()
                    .integer(),
                description: Joi.string(),
            }),
        }),
        (req, res, next) => floorController.patchFloor(req, res, next),
    )

    route.put(
        '/:id/floors/:floor',
        celebrate({
            body: Joi.object({
                floorNumber: Joi.number()
                    .integer()
                    .required(),
                description: Joi.string()
            }),
        }),
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
        (req, res, next) => buildingController.patchBuilding(req, res, next),
    )

    route.put(
        '/:id',
        celebrate({
            body: Joi.object({
                name: Joi.string().required(),
                description: Joi.string().required(),
                maxFloorDimensions: Joi.object({
                    length: Joi.number()
                        .integer()
                        .required(),
                    width: Joi.number()
                        .integer()
                        .required(),
                }).required(),
            }),
        }),
        (req, res, next) => buildingController.putBuilding(req, res, next),
    )

    route.post(
        '/:id/elevators',
        celebrate({
            body: Joi.object({
                floors: Joi.array()
                    .items(
                        Joi.number()
                            .integer()
                            .required(),
                    )
                    .required(),
                brand: Joi.string(),
                model: Joi.string(),
                serialNumber: Joi.string(),
                description: Joi.string(),
            }),
        }),
        (req, res, next) => elevatorCtrl.createElevator(req, res, next),
    )

    route.patch(
        '/:idb/elevators/:ide',
        celebrate({
            body: Joi.object({
                floors: Joi.array()
                    .items(
                        Joi.number()
                            .integer()
                            .required(),
                    )
                    .optional(),
                brand: Joi.string(),
                model: Joi.string(),
                serialNumber: Joi.string(),
                description: Joi.string(),
            }),
        }),
        (req, res, next) => elevatorCtrl.patchElevator(req, res, next),
    )

    route.put(
        '/:idb/elevators/:ide',
        celebrate({
            body: Joi.object({
                floors: Joi.array()
                    .items(
                        Joi.number()
                            .integer()
                            .required(),
                    )
                    .required(),
                brand: Joi.string(),
                model: Joi.string(),
                serialNumber: Joi.string(),
                description: Joi.string(),
            }),
        }),
        (req, res, next) => elevatorCtrl.putElevator(req, res, next),
    )

    route.post(
        '/:buildingId/floors/:floorNumber/rooms',
        celebrate({
            body: Joi.object({
                name:Joi.string().required(),
                description:Joi.string().required(),
                category:Joi.string().required(),
                dimensions: Joi.object({
                    length: Joi.number().integer(),
                    width: Joi.number().integer()
                }),
                positions: Joi.object({
                    x: Joi.number().integer(),
                    y: Joi.number().integer()
                }),
            }),
        }),
        (req, res, next) => roomCtrl.createRoom(req, res, next),
    )

    route.get(
        '/:id/elevators',
        (req, res, next) => elevatorCtrl.getElevators(req, res, next),
    )

    route.get(
        '',
        celebrate({
            query: {
                minFloors: Joi.number().integer(),
                maxFloors: Joi.number().integer(),
            },
        }),

        (req, res, next) => {
            if (parseInt(req.query.minFloors as string)>=0 && parseInt(req.query.maxFloors as string) >=0) {
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
                mapContent: Joi.array().items(Joi.array().items(Joi.number().required())),
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
        }), upload.single('file'),
        (req, res, next) => floorController.updateMap(req, res, next),
    )
    route.get(
        '/:id/floors/:floorNumber/map',
        (req, res, next) => floorController.getMap(req, res, next),
    )

    route.get('/:id/floors/passages', (req, res, next) => floorController.floorsWithPassage(req, res, next))
}
