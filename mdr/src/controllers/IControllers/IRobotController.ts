import { Request, Response, NextFunction } from 'express'

export default interface IRobotController {
    createRobot(req: Request, res: Response, next: NextFunction)
    inhibitRobot(req: Request, res: Response, next: NextFunction)
    getRobots(req: Request, res: Response, next: NextFunction)
}
