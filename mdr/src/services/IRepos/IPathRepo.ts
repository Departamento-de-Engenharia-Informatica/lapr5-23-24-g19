import { IPathBetweenBuildingsDTO } from '../../dto/IPathBetweenBuildingsDTO'
import { IPathCriterionDTO } from '../../dto/IPathCriterionDTO'
import { IPathDTO } from '../../dto/IPathDTO'

export default interface IPlanningAdapter {
    find(dto: IPathBetweenBuildingsDTO): Promise<IPathDTO[]>
    criteria(): Promise<IPathCriterionDTO[]>
}
