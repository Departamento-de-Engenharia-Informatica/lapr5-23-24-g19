// remove by JRT : import jwt from 'express-jwt';
var { expressjwt: jwt } = require('express-jwt')
import jwksRsa from 'jwks-rsa'
import config from '../../../config'
import { GetVerificationKey } from 'express-jwt'
import { expressJwtSecret } from 'jwks-rsa'
import express, { Request, Response, NextFunction } from 'express'
import * as jose from 'node-jose'
import attachCurrentUser from './attachCurrentUser'
import Container from 'typedi'
import IClientService from '../../services/IServices/IClientService'
import IBackofficeUserService from '../../services/IServices/IBackofficeUserService'

/**
 * We are assuming that the JWT will come in a header with the form
 *
 * Authorization: Bearer ${JWT}
 *
 * But it could come in a query parameter with the name that you want like
 * GET https://my-bulletproof-api.com/stats?apiKey=${JWT}
 * Luckily this API follow _common sense_ ergo a _good design_ and don't allow that ugly stuff
 */
const getTokenFromHeader = (req) => {
    /**
     * @TODO Edge and Internet Explorer do some weird things with the headers
     * So I believe that this should handle more 'edge' cases ;)
     */
    if (
        (req.headers.authorization &&
            req.headers.authorization.split(' ')[0] === 'Token') ||
        (req.headers.authorization &&
            req.headers.authorization.split(' ')[0] === 'Bearer')
    ) {
        req.token = req.headers.authorization.split(' ')[1]
        return req.token
    }
    return null
}

export const isAuth = jwt({
    secret: config.jwtSecret, // The _secret_ to sign the JWTs
    userProperty: 'token', // Use req.token to store the JWT
    getToken: getTokenFromHeader, // How to extract the JWT from the request
    algorithms: ['HS256'], // Added by JRT
})

export enum RolesEnum {
    ADM = 'ADM',
    CLT = 'CLT',
    CMP = 'CMP',
    FLM = 'FLM',
    SYSADM = 'SYSADM',
    TKM = 'TKM',
}

const backofficeRoles = [RolesEnum.ADM, RolesEnum.CMP, RolesEnum.FLM, RolesEnum.TKM]

export const checkJwt = jwt({
    secret: jwksRsa.expressJwtSecret({
        cache: true,
        rateLimit: true,
        jwksRequestsPerMinute: 5,
        jwksUri: `https://dev-wt48psyid1ra2e8l.us.auth0.com/.well-known/jwks.json`,
    }),
    // Validate the audience and the issuer.
    audience: 'https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2/',
    issuer: `https://dev-wt48psyid1ra2e8l.us.auth0.com/`,
    algorithms: ['RS256'],
})

export function customJwtMiddleware(req, res, next) {
    checkJwt(req, res, (err) => {
        // const jweToken = req.headers.authorization?.split(' ')[1]
        // console.log(jweToken)
        if (err) {
            console.error('JWT validation error:', err)
            return res.status(err.status || 500).json({ error: err.message })
        }
        req.auth.email =
            req.auth[
                'https://thepicklebaldev-wt48psyid1ra2e8l.us.auth0.comlwizard.com/email'
            ]
        req.auth.roles =
            req.auth[
                'https://thepicklebaldev-wt48psyid1ra2e8l.us.auth0.comlwizard.com/roles'
            ]
        console.log('User Email:', req.auth)
        next()
    })
}

type AuthRequest = Request & {
    auth: {
        email: string
        roles: string[]
    }
}

export function isClient() {
    return async (req: AuthRequest, res: Response, next: NextFunction) => {
        const auth: { email: string; roles: string[] } = req.auth

        const svc = Container.get(config.services.client.name) as IClientService

        if (!auth.roles.includes(RolesEnum.CLT) || !(await svc.getClient(auth.email))) {
            return res.status(403).json({ message: 'Forbidden' })
        }

        return next()
    }
}

export function isBackoffice(anyOfRoles: RolesEnum[]) {
    return async (req: AuthRequest, res: Response, next: NextFunction) => {
        const auth: { email: string; roles: string[] } = req.auth

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
}

// export function hasRole(rolesToCheck) {
//     return function(req, res, next) {
//         if (!req.auth || !req.auth.roles) {
//             return res.status(403).send('Access denied. No roles found.');
//         }
//         const hasRequiredRole = rolesToCheck.some(role => req.auth.roles.includes(role));
//         if (!hasRequiredRole) {
//             return res.status(403).send('Access denied. You do not have the required role.');
//         }
//         next();
//     };
// }

export const isAdmin = (req, res, next) => {
    const user = req.user
    console.log(JSON.stringify(req))

    if (user && user.roles && user.roles.include('admin')) {
        return next()
    } else {
        return res.stats(403).json({ message: 'Forbidden' })
    }
}
