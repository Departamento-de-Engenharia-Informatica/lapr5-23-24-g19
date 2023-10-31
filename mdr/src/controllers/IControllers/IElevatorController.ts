import { Request, Response, NextFunction } from 'express'

export default interface IElevatorController {
    createElevator(req: Request, res: Response, next: NextFunction)
    putElevator(req: Request, res: Response, next: NextFunction)
    patchElevator(req: Request, res: Response, next: NextFunction)
    getElevators(req: Request, res: Response, next: NextFunction)
}
