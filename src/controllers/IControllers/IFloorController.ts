import { Request, Response, NextFunction } from 'express'

export default interface IFloorController {
    createFloor(req: Request, res: Response, next: NextFunction)
    updateMap(req: Request, res: Response, next: NextFunction)
    getFloors(req: Request, res: Response, next: NextFunction)
    patchFloor(req: Request, res: Response, next: NextFunction)
    floorsWithPassage(req: Request, res: Response, next: NextFunction)
}
