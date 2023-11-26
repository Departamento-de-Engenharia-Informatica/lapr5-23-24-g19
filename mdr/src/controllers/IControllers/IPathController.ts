import { Request, Response, NextFunction } from 'express'

export default interface IPathController {
    pathsBetweenBuildings(req: Request, res: Response, next: NextFunction)
    getPathCriteria(req: Request, res: Response, next: NextFunction)
}
