import { IGetPathsDTO } from '../../dto/IGetPathsDTO'
import { IPathCriterionDTO } from '../../dto/IPathCriterionDTO'
import { IPathDTO } from '../../dto/IPathDTO'

export default interface IPathService {
    pathsBetweenBuildings(dto: IGetPathsDTO): Promise<IPathDTO[]>
    getPathCriteria(): Promise<IPathCriterionDTO[]>
}
