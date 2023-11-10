import { Request, Response, NextFunction } from 'express'

export default interface IRobotTypeController {
    createRobotType(req: Request, res: Response, next: NextFunction)
    getRobotTypes(req: Request, res: Response, next: NextFunction)
}
