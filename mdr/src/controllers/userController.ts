import { Response, NextFunction } from 'express'
import { AuthRequest } from '../api/middlewares/isAuth'

export async function getMe(req: AuthRequest, res: Response, _next: NextFunction) {
    // NB: a arquitetura ONION não está a ser seguida aqui

    // const userRepo = Container.get(config.repos.user.name) as IUserRepo

    // if (!req.token || req.oken == undefined)
    // return res.status(401).send('Token inexistente ou inválido')

    // const user = await userRepo.findById(req.user.id)
    console.log('Logging')

    if (req.auth) {
        const me: UserDTO = {
            email: req.auth.email,
            roles: req.auth.roles,
        }
        console.log(JSON.stringify(me))

        return res.json(me).status(200)
    }
    console.log('whatttt')

    return res.status(401).send('Auth token not found')
}

interface UserDTO {
    email: string
    roles: string[]
}
