import { Request, Response, NextFunction } from 'express'

export default interface IClientController {
    createClient(req: Request, res: Response, next: NextFunction)
    updateClientState(req: Request, res: Response, next: NextFunction)
    getClient(req: Request, res: Response, next: NextFunction)
    getClientsByState(req: Request, res: Response, next: NextFunction)
    patchClient(req: Request, res: Response, next: NextFunction)
    deleteClient(req: Request, res: Response, next: NextFunction)

    exportClientData(req: Request, res: Response, next: NextFunction)
}
