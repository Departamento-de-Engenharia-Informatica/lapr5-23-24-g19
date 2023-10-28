import { Either, Result } from '../../core/logic/Result'
import { IPassageDTO } from '../../dto/IPassageDTO'
import IUpdatePassageDTO from '../../dto/IUpdatePassageDTO'

export enum ErrorCode {
    NotFound,
    AlreadyExists,
    BussinessRuleViolation,
}

export type ErrorResult = {
    errorCode: ErrorCode
    message: string
}

export default interface IPassageService {
    createPassage(passageDTO: IPassageDTO): Promise<Either<ErrorResult, IPassageDTO>>
    editPassage(passageDTO: IUpdatePassageDTO): Promise<Either<ErrorResult, IPassageDTO>>
    getAllPassages(): Promise<Either<ErrorResult, IPassageDTO[]>>
    getPassagesBetweenBuildings(
        building1Code: string,
        building2Code: string,
    ): Promise<Either<ErrorResult, IPassageDTO[]>>
}
