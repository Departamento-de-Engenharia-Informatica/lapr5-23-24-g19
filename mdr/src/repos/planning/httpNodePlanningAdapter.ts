import config from '../../../config'
import { Service } from 'typedi'

import { IPathBetweenBuildingsDTO } from '../../dto/IPathBetweenBuildingsDTO'
import { IPathCriterionDTO } from '../../dto/IPathCriterionDTO'
import { IPathDTO } from '../../dto/IPathDTO'
import IPlanningAdapter from '../../services/IRepos/IPathRepo'

import fetch from 'node-fetch'

@Service()
export default class HttpNodePlanningAdapter implements IPlanningAdapter {
    private url = config.planningURL

    async find(dto: IPathBetweenBuildingsDTO): Promise<IPathDTO[]> {
        const res = await fetch(`${this.url}/paths`, {
            method: 'POST',
            headers: { 'Content-type': 'application/json' },
            body: JSON.stringify(dto),
        })

        if (!res.ok) {
            return []
        }

        const paths = (await res.json()) as IPathDTO[]
        return paths
    }

    async criteria(): Promise<IPathCriterionDTO[]> {
        const res = await fetch(`${this.url}/paths/criteria`)

        if (!res.ok) {
            return Promise.reject()
        }

        const criteria = (await res.json()) as IPathCriterionDTO[]
        return criteria
    }
}
