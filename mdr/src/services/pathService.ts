import { Inject, Service } from 'typedi'
import config from '../../config'
import { IPathBetweenBuildingsDTO } from '../dto/IPathBetweenBuildingsDTO'
import { IPathCriterionDTO } from '../dto/IPathCriterionDTO'
import { IPathDTO } from '../dto/IPathDTO'
import IPlanningAdapter from './IRepos/IPathRepo'
import IPathService from './IServices/IPathService'

@Service()
export default class PathService implements IPathService {
    constructor(@Inject(config.repos.path.name) private pathRepo: IPlanningAdapter) {}

    async pathsBetweenBuildings(dto: IPathBetweenBuildingsDTO): Promise<IPathDTO> {
        return this.pathRepo.find(dto)
    }

    async getPathCriteria(): Promise<IPathCriterionDTO[]> {
        return this.pathRepo.criteria()
    }
}
