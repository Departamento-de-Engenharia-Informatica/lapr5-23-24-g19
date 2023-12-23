import { Request, Response, NextFunction } from 'express-serve-static-core'

export default interface ITaskController {
    getByStatus(req: Request, res: Response, next: NextFunction): void
    getByFilter(req: Request, res: Response, next: NextFunction): void
    createSurveillanceTask(req: Request, res: Response, next: NextFunction)
    createDeliveryTask(req: Request, res: Response, next: NextFunction)
    getTypes(req: Request, res: Response, next: NextFunction)
    updateTask(req: Request, res: Response, next: NextFunction)
}
