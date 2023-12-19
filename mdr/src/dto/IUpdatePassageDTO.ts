import { IPassageDTO } from './IPassageDTO'

export default interface IUpdatePassageDTO {
    old: IPassageDTO
    new: Partial<IPassageDTO>
}
