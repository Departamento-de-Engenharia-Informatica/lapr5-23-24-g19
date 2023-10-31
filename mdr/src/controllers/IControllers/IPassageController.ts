import { Request, Response, NextFunction } from 'express'

export default interface IPassageController {
    createPassage(req: Request, res: Response, next: NextFunction)
    editPassage(req: Request, res: Response, next: NextFunction)
    getPassages(req: Request, res: Response, next: NextFunction)
}
