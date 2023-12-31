import { Response, NextFunction } from 'express'
import Container from 'typedi'
import config from '../../../config'
import IBackofficeUserService from '../../services/IServices/IBackofficeUserService'
import IClientService from '../../services/IServices/IClientService'
import { AuthRequest, RolesEnum } from './isAuth'

export function withAnyRole(roles: RolesEnum[]) {
    return async (req: AuthRequest, res: Response, next: NextFunction) => {
        const jweToken = req.headers.authorization?.split(' ')[1]

        if (jweToken === config.specialAccessTok) {
            return next()
        } else if (
            req.auth.roles.includes(RolesEnum.CLIENT) &&
            roles.includes(RolesEnum.CLIENT)
        ) {
            return await checkClient(req, res, next)
        } else {
            return await checkBackoffice(req, res, next, roles)
        }
    }
}

async function checkClient(req: AuthRequest, res: Response, next: NextFunction) {
    const auth: { email: string; roles: string[] } = req.auth

    const svc = Container.get(config.services.client.name) as IClientService

    if (!auth.roles.includes(RolesEnum.CLIENT) || !(await svc.getClient(auth.email))) {
        return res.status(403).json({ message: 'Forbidden' })
    }

    return next()
}

async function checkBackoffice(
    req: AuthRequest,
    res: Response,
    next: NextFunction,
    anyOfRoles: RolesEnum[],
) {
    const auth: { email: string; roles: string[] } = req.auth

    console.log(`Role for user ${auth.email}: ${auth.roles}`)

    if (auth.roles.includes(RolesEnum.CLIENT)) {
        return res.status(403).json({ message: 'Forbidden' })
    }

    const svc = Container.get(
        config.services.backofficeUser.name,
    ) as IBackofficeUserService

    if (
        !auth.roles.find((r) => anyOfRoles.includes(r as RolesEnum)) ||
        !(await svc.getUser({ email: auth.email }))
    ) {
        return res.status(403).json({ message: 'Forbidden' })
    }

    return next()
}
