import { Request, ParamsDictionary, Response, NextFunction } from 'express-serve-static-core'
import { ParsedQs } from 'qs'

export default interface ITaskController {
    createTask(req: Request, res: Response, next: NextFunction)
    getTypes(req: Request, res: Response, next: NextFunction)
}
