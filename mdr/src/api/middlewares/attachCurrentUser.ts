import { Container } from 'typedi'

import winston from 'winston'

import config from '../../../config'

import IUserRepo from '../../services/IRepos/IUserRepo'
import jwt from 'jsonwebtoken'
import { User } from '../../domain/user'
import { IUserPersistence } from '../../dataschema/mongo/IUserPersistence'
import { IUserDTO } from '../../dto/IUserDTO'

/**
 * Attach user to req.user
 * @param {*} req Express req Object
 * @param {*} res  Express res Object
 * @param {*} next  Express next Function
 */
const attachCurrentUser = async (req, res, next) => {
    const Logger = Container.get('logger') as winston.Logger
    try {
        const userRepo = Container.get(config.repos.user.name) as IUserRepo

        if (!req.token || req.token == undefined)
            next(new Error('Token inexistente ou inválido '))

        const jw = jwt.decode(req.token, { complete: true, json: true })
        const user = jw.payload as User
        const isFound = await userRepo.findById(user.id.toString()) != null

        if (isFound) {
            req.user = user
            next()
        }
        else
            next(new Error('Token não corresponde a qualquer utilizador do sistema'))
    } catch (e) {
        Logger.error('🔥 Error attaching user to req: %o', e)
        return next(e)
    }
}

const checkRole = async (requiredRoles: string[], req, res, next) => {
    const user: IUserDTO = req.user;

    console.log(req.user)
    if (!user || user === undefined)
        return res.status(401).json({ error: 'Forbidden' });

    if (requiredRoles.includes(user.role)) {
        return next();
    } else{
        return res.status(403).json({ error: 'Unauthorized' });
    }

};

enum Roles {
    FleetManager = "Fleet Manager"
}

const checkFleetManager = (req, res, next) => checkRole([Roles.FleetManager], req, res, next);

export { checkFleetManager }
export default attachCurrentUser
