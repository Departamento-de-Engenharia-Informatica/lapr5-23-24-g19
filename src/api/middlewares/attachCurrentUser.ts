import { Container } from 'typedi'

import winston from 'winston'

import config from '../../../config'

import IUserRepo from '../../services/IRepos/IUserRepo'
import jwt from 'jsonwebtoken'
import { User } from '../../domain/user'

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

        const id = req.token        
        const jw = jwt.decode(id,{complete: true,json: true})
        const user = jw.payload as User
        const isFound = await userRepo.exists(user)

        if (isFound)
            next()
        else next(new Error('Token não corresponde a qualquer utilizador do sistema'))
    } catch (e) {
        Logger.error('🔥 Error attaching user to req: %o', e)
        return next(e)
    }
}

export default attachCurrentUser
