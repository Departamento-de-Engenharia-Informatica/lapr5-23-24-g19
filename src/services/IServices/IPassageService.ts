import { Result } from '../../core/logic/Result'
import { IPassageDTO } from '../../dto/IPassageDTO'
import { IBuildingDTO } from '../../dto/IBuildingDTO'
import IUpdatePassageDTO from '../../dto/IUpdatePassageDTO'

export default interface IPassageService {
    createPassage(passageDTO: IPassageDTO): Promise<Result<IPassageDTO>>
    editPassage(passageDTO: IUpdatePassageDTO): Promise<Result<IPassageDTO>>
    getAllPassages(): Promise<Result<IPassageDTO[]>>
    getPassagesBetweenBuildings(building1Code: string, building2Code: string): Promise<Result<IPassageDTO[]>>
}
