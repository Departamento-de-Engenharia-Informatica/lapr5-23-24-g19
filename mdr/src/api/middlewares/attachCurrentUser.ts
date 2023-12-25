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
        const email = req.auth.email
        if (!email) {
            next(new Error('Email invalido'))
        }
        const user = await userRepo.findByEmail(email)

        if (user != null) {
            req.user = user
            next()
        } else next(new Error('Token não corresponde a qualquer utilizador do sistema'))
    } catch (e) {
        Logger.error('🔥 Error attaching user to req: %o', e)
        return next(e)
    }
}

const checkRole = async (requiredRoles: string[], req, res, next) => {
    const user: IUserDTO = req.user

    console.log(req.user)
    if (!user || user === undefined) return res.status(401).json({ error: 'Forbidden' })

    if (requiredRoles.includes(user.role)) {
        return next()
    } else {
        next(new Error('Unauthorized'))
    }
}

enum Roles {
    FleetManager = 'Fleet Manager',
}

const checkAdm = (req, res, next) => checkRole([config.systemRoles[0]], req, res, next)

export { checkAdm }
export default attachCurrentUser
