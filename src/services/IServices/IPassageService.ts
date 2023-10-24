import { Result } from '../../core/logic/Result'
import { IPassageDTO } from '../../dto/IPassageDTO'

export default interface IPassageService {
    createPassage(passageDTO: IPassageDTO): Promise<Result<IPassageDTO>>
}
