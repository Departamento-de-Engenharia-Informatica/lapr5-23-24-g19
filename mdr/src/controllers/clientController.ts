import config from '../../config'
import { Inject, Service } from 'typedi'
import { Request, Response, NextFunction } from 'express'

import IClientService, {
    ClientErrorCode,
    ClientErrorResult,
} from '../services/IServices/IClientService'
import IClientController from './IControllers/IClientController'
import { IClientDTO } from '../dto/IClientDTO'
import { ICreatedClientDTO } from '../dto/ICreatedClientDTO'
import { IDeletedClientDTO } from '../dto/IDeletedClientDTO'
import { IClientWithoutPasswordDTO } from '../dto/IClientWithoutPasswordDTO'
import IUpdateClientStateDTO from '../dto/IUpdateClientStateDTO'
import { IClientEmailDTO } from '../dto/IClientEmailDTO'
import { IClientDataRequestDTO } from '../dto/IClientDataRequestDTO'
import { IClientDataDTO } from '../dto/IClientDataDTO'
import IArchiveService from '../services/IServices/IArchiveService'

import * as fs from 'fs/promises'

@Service()
export default class ClientController implements IClientController {
    constructor(
        @Inject(config.services.client.name) private service: IClientService,
        @Inject(config.services.archive.name) private archiveSvc: IArchiveService,
    ) {}

    async createClient(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IClientDTO
            const result = await this.service.createClient(dto)

            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as ICreatedClientDTO
            return res.status(201).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async updateClientState(req: Request, res: Response, next: NextFunction) {
        try {
            const state = req.body.state as string
            if (state !== 'rejected' && state !== 'approved') {
                return res.status(400).send('Invalid state')
            }

            const dto = req.body as IUpdateClientStateDTO
            const result = await this.service.updateClientState(dto)

            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as ICreatedClientDTO
            return res.status(200).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async getClient(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.service.getClient(req.params.email as string)
            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as IClientDTO
            return res.status(200).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async getClientsByState(req: Request, res: Response, next: NextFunction) {
        try {
            const state = req.query.state as string
            if (state !== 'Rejected' && state !== 'Approved' && state !== 'Pending') {
                return res.status(400).send('Invalid state')
            }

            const result = await this.service.getClientsByState(state)
            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }
            const message = result.value as ICreatedClientDTO[]
            return res.status(200).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async patchClient(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IClientWithoutPasswordDTO
            dto.email = req.params.email as string

            const result = await this.service.patchClient(dto)
            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as ICreatedClientDTO
            return res.status(200).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async deleteClient(req: Request, res: Response, next: NextFunction) {
        try {
            const dto = req.body as IClientEmailDTO
            dto.email = req.params.email as string
            const result = await this.service.deleteClient(dto)

            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const message = result.value as IDeletedClientDTO
            return res.status(200).send(message)
        } catch (e) {
            return next(e)
        }
    }

    async exportClientData(req: Request, res: Response, next: NextFunction) {
        try {
            const dto: IClientDataRequestDTO = { email: req.body.email }

            const result = await this.service.getClientData(dto)

            if (result.isLeft()) {
                const err = result.value as ClientErrorResult
                return res
                    .status(this.resolveHttpCode(err.errorCode))
                    .send(JSON.stringify(err.message))
            }

            const data = result.value as IClientDataDTO

            const archivePath = await this.archiveSvc.createArchive(data, {
                workdirBase: dto.email,
            })

            return res.sendFile(archivePath, { root: './' }, async (err) => {
                if (err) {
                    console.error('Error sending ZIP file:', err)
                    res.status(500).send('Internal Server Error')
                } else {
                    // cleanup archive
                    await fs.unlink(archivePath)
                    console.log('Temporary files cleaned up successfully')
                }
            })
        } catch (e) {
            return next(e)
        }
    }

    private resolveHttpCode(result: ClientErrorCode) {
        let ret: number
        switch (result) {
            case ClientErrorCode.BussinessRuleViolation:
                ret = 422
                break
            case ClientErrorCode.NotFound:
                ret = 404
                break
            case ClientErrorCode.AlreadyExists:
                ret = 422
                break
            default:
                ret = 400
                break
        }
        return ret
    }
}
