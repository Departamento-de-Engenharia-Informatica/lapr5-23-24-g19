import { Request, Response, NextFunction } from 'express'

export default interface IClientController {
    createClient(req: Request, res: Response, next: NextFunction)
}
