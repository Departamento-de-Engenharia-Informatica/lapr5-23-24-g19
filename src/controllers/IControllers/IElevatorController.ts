import { Request, Response, NextFunction } from 'express'

export default interface IElevatorController {
    createElevator(req: Request, res: Response, next: NextFunction)
}
