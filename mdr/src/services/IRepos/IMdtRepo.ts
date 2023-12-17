import { IPathBetweenBuildingsDTO } from "../../dto/IPathBetweenBuildingsDTO";
import { IPathCriterionDTO } from "../../dto/IPathCriterionDTO";
import { IPathDTO } from "../../dto/IPathDTO";

export default interface IMdtAdapter {
    create(String): Promise<String>
}
