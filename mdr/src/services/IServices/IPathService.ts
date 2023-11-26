import { IPathBetweenBuildingsDTO } from "../../dto/IPathBetweenBuildingsDTO";
import { IPathCriterionDTO } from "../../dto/IPathCriterionDTO";
import { IPathDTO } from "../../dto/IPathDTO";

export default interface IPathService {
    pathsBetweenBuildings(dto: IPathBetweenBuildingsDTO): Promise<IPathDTO>
    getPathCriteria(): Promise<IPathCriterionDTO[]>
}
