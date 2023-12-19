import { Request, Response, NextFunction } from 'express-serve-static-core'

export default interface ITaskController {
    createSurveillanceTask(req: Request, res: Response, next: NextFunction)
    createDeliveryTask(req: Request, res: Response, next: NextFunction)
    getTypes(req: Request, res: Response, next: NextFunction)
}
