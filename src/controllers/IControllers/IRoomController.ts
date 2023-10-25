import { Request, Response, NextFunction } from 'express'

export default interface IRoomController {
    createRoom(req: Request, res: Response, next: NextFunction)
}
