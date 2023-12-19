import { Service } from 'typedi'
import config from '../../../config'

import fetch from 'node-fetch'
import { ITaskDTO } from '../../dto/ITaskDTO'
import IMdtAdapter from '../../services/IRepos/IMdtRepo'

@Service()
export default class HttpNodeMdtAdapter implements IMdtAdapter {
    private url = config.mdtURL
    async createSurveillanceTask(dto: ITaskDTO): Promise<String> {
        console.log('===============')
        console.log(dto)
        const res = await fetch(`${this.url}/jobs`, {
            method: 'POST',
            headers: { 'Content-type': 'application/json' },
            body: JSON.stringify(dto),
        })

        if (!res.ok) {
            return null
        }

        const paths = await res.json()

        return paths
    }

    async createDeliveryTask(dto: ITaskDTO): Promise<String> {
        console.log('===============')
        console.log(dto)
        const res = await fetch(`${this.url}/jobs`, {
            method: 'POST',
            headers: { 'Content-type': 'application/json' },
            body: JSON.stringify(dto),
        })

        if (!res.ok) {
            return null
        }

        const paths = await res.json()

        return paths
    }
}
