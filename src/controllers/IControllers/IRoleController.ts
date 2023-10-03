import { Request, Response, NextFunction } from 'express';

export default interface IRoleController  {
  createRole(req: Request, res: Response, next: NextFunction);
  updateRole(req: Request, res: Response, next: NextFunction);
}