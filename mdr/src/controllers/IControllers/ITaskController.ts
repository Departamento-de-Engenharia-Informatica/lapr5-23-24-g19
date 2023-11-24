import { Request, Response, NextFunction } from 'express'

export default interface ITaskController {
    getTypes(req: Request, res: Response, next: NextFunction)
}
