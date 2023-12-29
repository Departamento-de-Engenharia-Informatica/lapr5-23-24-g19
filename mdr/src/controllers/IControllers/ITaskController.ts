import { Request, Response, NextFunction } from 'express'

export default interface ITaskController {
    getByStatus(req: Request, res: Response, next: NextFunction): void
    getByFilter(req: Request, res: Response, next: NextFunction): void
    createSurveillanceTask(req: Request, res: Response, next: NextFunction)
    createDeliveryTask(req: Request, res: Response, next: NextFunction)
    getTypes(req: Request, res: Response, next: NextFunction)
    updateTask(req: Request, res: Response, next: NextFunction)

    taskSequence(req: Request, res: Response, next: NextFunction)
    taskSequenceAlgorithms(req: Request, res: Response, next: NextFunction)
}
