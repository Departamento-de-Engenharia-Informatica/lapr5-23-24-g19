import { Service } from 'typedi'
import config from '../../../config'

import fetch from 'node-fetch'
import { ITaskDTO } from '../../dto/ITaskDTO'
import IMdtAdapter from '../../services/IRepos/IMdtRepo'
import { IFilterDTO } from '../../dto/IFilterDTO'
import { IUpdateTaskDTO } from '../../dto/IUpdateTaskDTO'

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
    async getByFilter(dto: IFilterDTO): Promise<String> {
        var a = dto.criteria.toUpperCase()
        const res = await fetch(`${this.url}/jobs/filter?filter=${a}&rule=${dto.rule}`)

        if (!res.ok) {
            return Promise.reject()
        }

        return await res.json()
    }

    async getByStatus(status: string): Promise<String> {
        const res = await fetch(`${this.url}/jobs?status=${status}`)

        if (!res.ok) {
            return Promise.reject()
        }

        return await res.json()
    }

    async updateTask(dto: IUpdateTaskDTO): Promise<string> {
        const { id, ...body } = dto
        const res = await fetch(`${this.url}/jobs/${id}`, {
            method: 'PATCH',
            headers: { 'Content-type': 'application/json' },
            body: JSON.stringify({
                JobStatus: body.taskStatus,
            }),
        })

        if (!res.ok) {
            return Promise.reject(await res.text())
        }

        return await res.json()
    }
}
