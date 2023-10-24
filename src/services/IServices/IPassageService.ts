import { Result } from '../../core/logic/Result'
import { IPassageDTO } from '../../dto/IPassageDTO'
import { IBuildingDTO } from '../../dto/IBuildingDTO'

export default interface IPassageService {
    createPassage(passageDTO: IPassageDTO): Promise<Result<IPassageDTO>>
    getAllPassages(): Promise<Result<IPassageDTO[]>>
    getPassagesBetweenBuildings(building1Code: string, building2Code: string): Promise<Result<IPassageDTO[]>>
}
