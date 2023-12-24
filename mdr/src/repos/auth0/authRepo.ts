import { Service } from 'typedi'
import config from '../../../config'
import fetch from 'node-fetch'
import { IAuthUserDTO } from '../../dto/IAuthUserDTO'
import IAuthRepo from '../../services/IRepos/IAuthRepo'

@Service()
export default class AuthRepo implements IAuthRepo {
    async createUser(dto: IAuthUserDTO): Promise<String> {
        const res = await fetch(`${config.auth0.audience}/users`, {
            method: 'POST',
            headers: {
                'Content-type': 'application/json',
                Authorization: `Bearer ${config.auth0.bearer}`,
            },
            body: JSON.stringify(dto),
        })

        if (!res.ok) {
            return Promise.reject()
        }

        return await res.json()
    }
}
