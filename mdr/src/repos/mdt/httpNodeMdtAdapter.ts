import { Service } from 'typedi'
import config from '../../../config'

import fetch from 'node-fetch'
import { ITaskDTO } from '../../dto/ITaskDTO'
import IMdtAdapter from '../../services/IRepos/IMdtRepo'
import { IFilterDTO } from '../../dto/IFilterDTO'
import { IUpdateTaskDTO } from '../../dto/IUpdateTaskDTO'
import { IRobotTasksDTO } from '../../dto/IRobotTasksDTO'
import { ISequenceAlgorithmDTO } from '../../dto/ISequenceAlgorithmDTO'
import { IRobotTaskSequenceDTO } from '../../dto/IRobotTaskSequenceDTO'
import { IClientTasksRequestDTO } from '../../dto/IClientTasksRequestDTO'
import { IClientTaskDTO } from '../../dto/IClientTaskDTO'
import { IUpdatedTaskDTO } from '../../dto/IUpdatedTaskDTO'

@Service()
export default class HttpNodeMdtAdapter implements IMdtAdapter {
    private url = config.mdtURL

    async createSurveillanceTask(dto: ITaskDTO): Promise<String> {
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
            return Promise.reject(await res.text())
        }

        return await res.json()
    }

    async getByStatus(status: string): Promise<string[]> {
        const res = await fetch(`${this.url}/jobs?status=${status}`)

        if (!res.ok) {
            return Promise.reject()
        }

        return await res.json()
    }

    async updateTask(dto: IUpdateTaskDTO): Promise<IUpdatedTaskDTO> {
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

        return (await res.json()) as IUpdatedTaskDTO
    }

    async taskSequence(dto: IRobotTasksDTO): Promise<IRobotTaskSequenceDTO> {
        console.log('===============')
        console.log(dto)
        const res = await fetch(`${this.url}/jobs/sequence`, {
            method: 'PATCH',
            headers: { 'Content-type': 'application/json' },
            body: JSON.stringify(dto),
        })

        if (!res.ok) {
            return Promise.reject(await res.text())
        }

        const sequence = (await res.json()) as IRobotTaskSequenceDTO
        return sequence
    }

    async getTaskSequenceAlgorithms(): Promise<ISequenceAlgorithmDTO[]> {
        const res = await fetch(`${this.url}/jobs/sequence/algorithms`)

        if (!res.ok) {
            return Promise.reject(await res.text())
        }

        return await res.json()
    }

    async getClientRequestedTasks(
        dto: IClientTasksRequestDTO,
    ): Promise<IClientTaskDTO[]> {
        const tasks = (await this.getByFilter({
            criteria: 'client',
            rule: dto.email,
        })) as unknown as any[]

        const taskDtos = tasks.map((t) => {
            const { id: _1, email: _2, ...task } = t
            return task as IClientTaskDTO
        })

        return taskDtos
    }
}
