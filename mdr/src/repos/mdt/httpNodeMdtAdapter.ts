import config from '../../../config'
import { Service } from 'typedi'

import { IPathBetweenBuildingsDTO } from '../../dto/IPathBetweenBuildingsDTO'
import { IPathCriterionDTO } from '../../dto/IPathCriterionDTO'
import { IPathDTO } from '../../dto/IPathDTO'
import IPlanningAdapter from '../../services/IRepos/IPathRepo'

import fetch from 'node-fetch'
import IMdtAdapter from '../../services/IRepos/IMdtRepo'
import path from 'path'

@Service()
export default class HttpNodeMdtAdapter implements IMdtAdapter {
    private url = config.mdtURL
    async create(dto: string): Promise<String> {
        // const res = await fetch(`${this.url}/paths`, {
        //     method: 'POST',
        //     headers: { 'Content-type': 'application/json' },
        //     body: JSON.stringify(dto),
        // })

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
