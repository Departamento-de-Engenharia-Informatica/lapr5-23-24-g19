// remove by JRT : import jwt from 'express-jwt';
const { expressjwt: jwt } = require('express-jwt')
import jwksRsa from 'jwks-rsa'
import argon2 from 'argon2'
import config from '../../../config'
import { Request, Response, NextFunction } from 'express'
import Container from 'typedi'
import IClientRepo from '../../services/IRepos/IClientRepo'
import { Email } from '../../domain/user/email'

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
    ADMIN = 'ADM',
    CLIENT = 'CLT',
    CAMPUS_MNG = 'CMP',
    FLEET_MNG = 'FLM',
    SYS_ADMIN = 'SYSADM',
    TASK_MNG = 'TKM',
}

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

export function customJwtMiddleware(req: AuthRequest, res: Response, next: NextFunction) {
    checkJwt(req, res, (err) => {
        const jweToken = req.headers.authorization?.split(' ')[1]

        if (
            jweToken != undefined &&
            jweToken != null &&
            jweToken != config.specialAccessTok
        ) {
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
        }
        next()
    })
}

export type AuthRequest = Request & {
    auth: {
        email: string
        roles: string[]
    }
}

export function requireReAuth() {
    return async (req: AuthRequest, res: Response, next: NextFunction) => {
        const pwd = req.body.password

        const repo = Container.get(config.repos.client.name) as IClientRepo
        const client = await repo.find(Email.create(req.body.email).getOrThrow())

        const pass = client._get_pwd()

        if (
            pass.isAlreadyHashed()
                ? !(await argon2.verify(pass.value, pwd))
                : pass.value !== pwd
        ) {
            return res.status(403).send(JSON.stringify({ message: 'Bad Password' }))
        }

        req.body.password = undefined
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

// export const isAdmin = (req, res, next) => {
//     const user = req.user
//     console.log(JSON.stringify(req))

//     if (user && user.roles && user.roles.include('admin')) {
//         return next()
//     } else {
//         return res.stats(403).json({ message: 'Forbidden' })
//     }
// }
