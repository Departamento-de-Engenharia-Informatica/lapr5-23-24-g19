import { Request, Response, NextFunction } from 'express'

export default interface IBuildingController {
    createBuilding(req: Request, res: Response, next: NextFunction)
    putBuilding(req: Request, res: Response, next: NextFunction)
    patchBuilding(req: Request, res: Response, next: NextFunction)
    getBuildings(req: Request, res: Response, next: NextFunction)
    getBuildingsByFloors(req: Request, res: Response, next: NextFunction)
}
