import { Request, Response, NextFunction } from 'express'

export default interface IRobotController {
    createRobot(req: Request, res: Response, next: NextFunction)
}
