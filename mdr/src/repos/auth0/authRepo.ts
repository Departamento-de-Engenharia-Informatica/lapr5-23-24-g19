import { Service } from 'typedi'
import config from '../../../config'
import fetch from 'node-fetch'
import { IAuthUserDTO } from '../../dto/IAuthUserDTO'
import IAuthRepo from '../../services/IRepos/IAuthRepo'
import { IAssingRoleDTO } from '../../dto/IAssignRoleDTO'

@Service()
export default class AuthRepo implements IAuthRepo {
    async createUser(dto: IAuthUserDTO): Promise<string> {
        const res = await fetch(`${config.auth0.audience}/users`, {
            method: 'POST',
            headers: {
                'Content-type': 'application/json',
                Authorization: `Bearer ${config.auth0.bearer}`,
            },
            body: JSON.stringify(dto),
        })
        if (!res.ok) {
            return Promise.reject(await res.text())
        }

        return await res.json()
    }

    async blockUser(email: string): Promise<void> {
        const user = await this.getUserByEmail(email)
        const res = await fetch(`${config.auth0.audience}/users/${user[0].user_id}`, {
            method: 'PATCH',
            headers: {
                'Content-type': 'application/json',
                Authorization: `Bearer ${config.auth0.bearer}`,
            },
            body: JSON.stringify({
                blocked: true,
            }),
        })

        if (res.status !== 200) {
            return Promise.reject()
        }
    }

    async unblockUser(email: string): Promise<void> {
        const user = await this.getUserByEmail(email)
        const res = await fetch(`${config.auth0.audience}/users/${user[0].user_id}`, {
            method: 'PATCH',
            headers: {
                'Content-type': 'application/json',
                Authorization: `Bearer ${config.auth0.bearer}`,
            },
            body: JSON.stringify({
                blocked: false,
            }),
        })

        if (res.status !== 200) {
            return Promise.reject()
        }
    }

    async deleteUser(email: string): Promise<void> {
        const user = await this.getUserByEmail(email)
        const res = await fetch(`${config.auth0.audience}/users/${user[0].user_id}`, {
            method: 'DELETE',
            headers: {
                'Content-type': 'application/json',
                Authorization: `Bearer ${config.auth0.bearer}`,
            },
        })

        if (res.status !== 204) {
            return Promise.reject()
        }
    }

    async getUserByEmail(email: string): Promise<IAuthUserDTO> {
        const res = await fetch(
            `${config.auth0.audience}/users-by-email?email=${email}`,
            {
                method: 'GET',
                headers: {
                    'Content-type': 'application/json',
                    Authorization: `Bearer ${config.auth0.bearer}`,
                },
            },
        )
        if (!res.ok) {
            return Promise.reject()
        }
        return await res.json()
    }

    async assignRoleToUser(dto: IAssingRoleDTO): Promise<void> {
        let role: string
        switch (dto.role.toUpperCase()) {
            case 'CLIENT':
                role = 'rol_IEg5oxECoH0vC4YG'
                break
            case 'CAMPUS MANAGER':
                role = 'rol_ceCbrjtQ7xaoEvLO'
                break
            case 'FLEET MANAGER':
                role = 'rol_Hm2DPPoiTtpTM7v4'
                break
            case 'TASK MANAGER':
                role = 'rol_0V4xenwghl11vyTa'
                break
            case 'ADMINISTRATOR':
                role = 'rol_3XQwx1275JW2djIl'
                break
            case 'SYSTEMS ADMINISTRATOR':
                role = 'rol_0EMKMFloiltoyKQn'
                break
        }

        const user = await this.getUserByEmail(dto.email)

        const res = await fetch(
            `${config.auth0.audience}/users/${user[0].user_id}/roles`,
            {
                method: 'POST',
                headers: {
                    'Content-type': 'application/json',
                    Authorization: `Bearer ${config.auth0.bearer}`,
                },
                body: JSON.stringify({
                    roles: [role],
                }),
            },
        )

        if (res.status !== 204) {
            return Promise.reject()
        }
    }
}
