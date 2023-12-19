import { Response, Request } from 'express'

import { Container } from 'typedi'

import config from '../../config'

import IUserRepo from '../services/IRepos/IUserRepo'

import { UserMap } from '../mappers/UserMap'
import { IUserDTO } from '../dto/IUserDTO'

exports.getMe = async function (req, res: Response) {
    // NB: a arquitetura ONION não está a ser seguida aqui

    const userRepo = Container.get(config.repos.user.name) as IUserRepo

    if (!req.token || req.token == undefined)
        return res.status(401).send('Token inexistente ou inválido')

    const user = await userRepo.findById(req.user.id)
    if (!user) {
        return res.status(401).send('Utilizador não registado')
    }

    const userDTO = UserMap.toDTO(user) as IUserDTO
    return res.json(userDTO).status(200)
}
