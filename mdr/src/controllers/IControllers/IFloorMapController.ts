import { NextFunction, Request, Response } from 'express'

export default interface IFloorMapController {
    getMap(req: Request, res: Response, next: NextFunction)
    updateMap(req: Request, res: Response, next: NextFunction)
}
