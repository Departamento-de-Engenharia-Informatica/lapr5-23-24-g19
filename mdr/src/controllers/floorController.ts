import { Request, Response, NextFunction } from 'express'
import { Inject, Service } from 'typedi'
import config from '../../config'

import IFloorController from './IControllers/IFloorController'
import IFloorService from '../services/IServices/IFloorService'

import { Result } from '../core/logic/Result'
import { IFloorDTO } from '../dto/IFloorDTO'
import { IFloorMapDTO } from '../dto/IFloorMapDTO'
import { IBuildingCodeDTO } from '../dto/IBuildingCodeDTO'
import { IUpdateFloorDTO } from '../dto/IUpdateFloorDTO'
import { ErrorCode, ErrorResult } from '../services/IServices/IFloorService'
import fs from 'fs'

@Service()
export default class FloorController implements IFloorController {
    constructor(@Inject(config.services.floor.name) private floorServiceInstance: IFloorService) {}

    public async createFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingId = req.params.id

            const result = await this.floorServiceInstance.createFloor(req.body as IFloorDTO, buildingId)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IFloorDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    public async patchFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IUpdateFloorDTO
            dto.oldFloorNumber = parseInt(req.params.floor)
            dto.buildingCode = req.params.id

            const result = await this.floorServiceInstance.patchFloor(req.body as IUpdateFloorDTO)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IUpdateFloorDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async putFloor(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IUpdateFloorDTO
            dto.oldFloorNumber = parseInt(req.params.floor)
            dto.buildingCode = req.params.id

            const result = await this.floorServiceInstance.putFloor(req.body as IUpdateFloorDTO)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IUpdateFloorDTO
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getFloors(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.floorServiceInstance.getFloors(req.params.id)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }

            const message = result.value as IFloorDTO[]
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async updateMap(req: Request, res: Response, next: NextFunction) {
        const uploadedFile = req.body
        try {
            const dto = req.body as IFloorMapDTO
            dto.buildingCode = req.params.id
            dto.floorNumber = parseInt(req.params.floorNumber)

            const result = await this.floorServiceInstance.uploadMap(dto)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }
            const message = result.value as IFloorMapDTO
            writeFile(JSON.stringify(uploadedFile, null, 2), message.path)
            return res.json(message).status(200)
        } catch (e) {
            return next(e)
        }
    }

    async floorsWithPassage(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = { code: req.params.id } as IBuildingCodeDTO

            const floors = await this.floorServiceInstance.floorsWithPassage(dto)

            if (floors.isFailure) {
                return res.status(404).send()
            }

            return res.json(floors.getValue()).status(200)
        } catch (e) {
            return next(e)
        }
    }

    public async getMap(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = { buildingCode: req.params.id, floorNumber: req.params.floorNumber }
            const result = await this.floorServiceInstance.getMap(dto as unknown as IFloorDTO)

            if (result.isLeft()) {
                const error = result.value as ErrorResult
                let ret: number = this.resolveHttpCode(error.errorCode)
                return res.status(ret).send(error.message)
            }
            if (result.isRight()) {
                //SHOULD THIS feature just return the path??, for now it is return the entire file
                readFile(result.value as string)
                    .then((fileContents) => {
                        return res.json(JSON.parse(fileContents)).status(200)
                    })
                    .catch((error) => {
                        console.error(error)
                        return res.status(500).send('Error reading file')
                    })
            }
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: ErrorCode) {
        let ret: number
        switch (result) {
            case ErrorCode.BusinessRuleViolation:
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

function writeFile(data: string, filePath: string): boolean {
    const uploadDir = 'maps/'

    if (!fs.existsSync(uploadDir)) {
        fs.mkdirSync(uploadDir)
    }

    const buildingPath = `${uploadDir}${filePath.split('/')[0]}`
    if (!fs.existsSync(buildingPath)) {
        fs.mkdirSync(buildingPath)
        console.log('[FloorMap] Directory created:', buildingPath)
    }

    // Check if the file already exists
    if (!fs.existsSync(`${uploadDir}${filePath}`)) {
        // Write to the file only if it doesn't exist
        fs.writeFile(`${uploadDir}${filePath}`, data, (err) => {
            if (err) {
                console.error('Error writing to file:', err)
                return false
            } else {
                return true
            }
        })
    }
    return false
}

function readFile(filePath: string): Promise<string> {
    const uploadDir = 'maps/'

    return new Promise((resolve, reject) => {
        // Read the file asynchronously
        fs.readFile(`${uploadDir}${filePath}`, 'utf8', (err, data) => {
            if (err) {
                reject(err)
            } else {
                resolve(data)
            }
        })
    })
}
