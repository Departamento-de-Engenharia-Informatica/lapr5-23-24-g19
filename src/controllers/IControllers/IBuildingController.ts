import { Request, Response, NextFunction } from 'express'

export default interface IRoleController {
    createBuilding(req: Request, res: Response, next: NextFunction)
}
