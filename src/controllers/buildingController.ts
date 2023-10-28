import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IBuildingController from './IControllers/IBuildingController'
import IBuildingService, { ErrorResult, ErrorCode } from '../services/IServices/IBuildingService'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { BuildingCode } from '../domain/building/buildingCode'
import { ParamsDictionary } from 'express-serve-static-core'
import { ParsedQs } from 'qs'
import { parseInt } from 'lodash'
import { IBuildingMinMaxFloorsDTO } from '../dto/IBuildingMinMaxFloorsDTO'
import { Either,left,right } from '../core/logic/Result'


@Service()
export default class BuildingController implements IBuildingController {
    constructor(@Inject(config.services.building.name) private service: IBuildingService) {}

    public async createBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.createBuilding(req.body as IBuildingDTO)

            if (result.isLeft()) {
                const error= result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }
            const message = result.value as IBuildingDTO
            return res.json(message).status(201);
        } catch (e) {
            return next(e)
        }
    }

    public async editBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IBuildingDTO;
            dto.code = req.params.id;
            const result = await this.service.createBuilding(dto)

            if (result.isLeft()) {
                const error= result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IBuildingDTO
            return res.json(message).status(200);
        } catch (e) {
            return next(e)
        }
    }

    public async patchBuilding(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.editBuilding(req.params.id, req.body as IBuildingDTO)

 
            if (result.isLeft()) {
                const error= result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IBuildingDTO
            return res.json(message).status(200);
        } catch (e) {
            return next(e)
        }
    }

    public async getBuildings(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getBuildings()
            if (result.isFailure) {
                return res.status(422).send()
            }
            return res.json(result.getValue()).status(200);
        } catch (e) {
            return next(e)
        }
    }

    public async getBuildingsByFloors(req: Request, res: Response, next: NextFunction) {
        try {
            const minFloors = parseInt(req.query.minFloors.toString())
            const maxFloors = parseInt(req.query.maxFloors.toString())

            if (minFloors > maxFloors) {
                return res.status(400).json({ error: 'minFloors cannot be greater than maxFloors' });
            }

            const dto = req.body as IBuildingMinMaxFloorsDTO
            dto.minMaxFloors = {min: minFloors, max: maxFloors}

            const result = await this.service.getBuildingsByFloors(dto);

            if (result.isFailure) {
                return res.status(404).send()
            }

            return res.json(result.getValue()).status(200);
        } catch (e) {
            throw e
        }
    }


    private resolveHttpCode(result: ErrorCode) {
        let ret: number
        switch (result) {
            case ErrorCode.BussinessRuleViolation:
                ret = 422
                break
            case ErrorCode.NotFound:
                ret = 404
                break
            default:
                ret = 400
                break
        }
        return ret
    }

}
