import { Request, Response, NextFunction } from 'express'

export default interface IBackofficeUserController {
    createBackofficeUser(req: Request, res: Response, next: NextFunction)
}
